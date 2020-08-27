use super::utils::*;

#[cfg(windows)]
pub mod __internal {
    use super::*;
    pub const PROT_READ: c_int = 0x1;
    pub const PROT_WRITE: c_int = 0x2;
    pub const PROT_EXEC: c_int = 0x4;
    pub const PROT_NONE: c_int = 0x0;

    pub const MAP_SHARED: c_int = 0x01;
    pub const MAP_PRIVATE: c_int = 0x02;
    pub const MAP_FAILED: *const u8 = -1i32 as *const u8;
    pub const MAP_FIXED: c_int = 0x10;
    pub const MAP_ANONYMOUS: c_int = 0x20;
}

#[cfg(windows)]
pub use __internal::*;
#[cfg(not(windows))]
pub use libc::{
    MAP_ANONYMOUS, MAP_FAILED, MAP_FIXED, MAP_PRIVATE, MAP_SHARED, PROT_EXEC, PROT_NONE, PROT_READ,
    PROT_WRITE,
};
pub struct MemMap {
    pub begin: Address,
    pub size: usize,
    pub base_begin: Address,
    pub base_size: usize,
    pub prot: i32,
    /// When reuse is true, this is just a view of an existing mapping
    /// and we do not take ownership and are not responsible for
    /// unmapping.
    pub reuse: bool,

    /// When already_unmapped is true the destructor will not call munmap.
    pub already_unmapped: bool,
    pub redzone_size: usize,
}

#[cfg(unix)]
impl MemMap {
    pub fn target_mmap_init() {
        // no-op for unix
    }

    pub fn target_mmap(
        start: Address,
        len: usize,
        prot: i32,
        flags: i32,
        fd: i32,
        off: libc::off_t,
    ) -> Address {
        unsafe { Address::from_ptr(libc::mmap(start.to_mut_ptr(), len, prot, flags, fd, off)) }
    }

    pub fn target_munmap(start: Address, len: usize) -> i32 {
        unsafe { libc::munmap(start.to_mut_ptr(), len) }
    }
}
#[allow(dead_code)]
static mut ALLOC_GRANULARITY: usize = 0;

#[cfg(windows)]
impl MemMap {
    pub fn null() -> Self {
        Self {
            base_begin: Address::null(),
            begin: Address::null(),
            base_size: 0,
            size: 0,
            prot: 0,
            redzone_size: 0,
            already_unmapped: true,
            reuse: false,
        }
    }
    pub fn target_mmap_init() {
        use winapi::um::sysinfoapi::*;

        let mut si: SYSTEM_INFO = core::mem::MaybeUninit::uninit();
        unsafe {
            GetSystemInfo(si.as_mut_ptr());
            ALLOC_GRANULARITY = si.dwAllocationGranularity as usize;
        }
    }

    pub fn target_mmap(
        _: Address,
        len: usize,
        prot: i32,
        flags: i32,
        fd: i32,
        off: libc::off_t,
    ) -> Address {
        use um::memoryapi::*;
        use um::winnt::*;
        use winapi::*;
        unsafe {
            let padding = off as usize % ALLOC_GRANULARITY;
            let file_offset = off - padding;
            let map_length = len + padding;
            if prot != PROT_READ && (prot != (PROT_READ | PROT_WRITE)) {
                panic!("protection or flag error was not supported");
            }

            if (flags & MAP_FIXED) != 0 {
                panic!("MAP_FIXED not supported");
            }

            let mut map_access = 0;
            let mut view_access = 0;
            if (prot & PROT_WRITE) != 0 {
                map_access = PAGE_READWRITE;
                if (flags & MAP_SHARED) != 0 && (flags & MAP_PRIVATE) != 0 {
                    view_access = FILE_MAP_ALL_ACCESS;
                } else if (flags & MAP_SHARED == 0) && (flags & MAP_PRIVATE) != 0 {
                    view_access = FILE_MAP_COPY | FILE_MAP_READ;
                } else {
                    panic!("MAP_PRIVATE and MAP_SHARED inconsistently set");
                }
            } else {
                map_access = PAGE_READONLY;
                view_access = FILE_MAP_READ;
            }

            todo!("WINAPI support");
        }
    }

    fn target_munmap(start: Address, len: usize) -> i32 {
        0
    }
}

#[cfg(target_os = "linux")]
pub const MADVISE_ZEROES: bool = true;
#[cfg(not(target_os = "linux"))]
pub const MADVISE_ZEROES: bool = false;

impl MemMap {
    pub const fn is_valid(&self) -> bool {
        self.base_size != 0
    }

    pub const fn get_protect(&self) -> i32 {
        self.prot
    }

    pub const fn begin(&self) -> Address {
        self.begin
    }

    pub const fn size(&self) -> usize {
        self.size
    }

    pub const fn end(&self) -> Address {
        self.begin().offset(self.size())
    }

    pub const fn base_begin(&self) -> Address {
        self.base_begin
    }

    pub const fn base_size(&self) -> usize {
        self.base_size
    }

    pub const fn base_end(&self) -> Address {
        self.base_begin().offset(self.base_size())
    }

    pub const fn has_address(&self, addr: Address) -> bool {
        (self.begin().to_usize() <= addr.to_usize()) & (addr.to_usize() < self.end().to_usize())
    }

    pub fn map_anonymous(
        name: &str,
        addr: Address,
        byte_count: usize,
        prot: i32,
        reuse: bool,
        reservation: Option<&mut Self>,
    ) -> Self {
        let page_aligned_byte_count: usize = round_up(byte_count, crate::utils::os::page_size());

        let mut flags = MAP_PRIVATE | MAP_ANONYMOUS;
        if reuse {
            assert!(addr.is_non_null());
            assert!(reservation.is_none());
            flags |= MAP_FIXED;
        } else if let Some(_) = reservation {
            flags |= MAP_FIXED;
        }

        let fd = -1;
        let actual = Self::target_mmap(addr, page_aligned_byte_count, prot, flags, fd, 0);
        if actual.to_ptr() == MAP_FAILED {
            panic!("mmap failed");
        }
        Self::new(
            name,
            actual,
            byte_count,
            actual,
            page_aligned_byte_count,
            prot,
            reuse,
            0,
        )
    }

    pub fn do_reset(&mut self) {
        debug_assert!(self.is_valid());

        if !self.reuse {
            if !self.already_unmapped {
                let result = Self::target_munmap(self.base_begin, self.base_size);
                if result == -1 {
                    panic!("munmap failed");
                }
            }
        }
        self.base_size = 0;
    }

    pub fn reset(&mut self) {
        if self.is_valid() {
            self.do_reset();
        }
    }
    pub fn new(
        name: &str,
        begin: Address,
        size: usize,
        base_begin: Address,
        base_size: usize,
        prot: i32,
        reuse: bool,
        redzone: usize,
    ) -> Self {
        Self {
            begin: begin,
            size,
            base_begin: base_begin,
            base_size: base_size,
            prot,
            redzone_size: redzone,
            already_unmapped: false,
            reuse,
        }
    }

    pub fn madvise_dontneed_and_zero(&self) {
        debug_assert!(self.base_begin().is_non_null() && self.base_size() != 0);

        #[cfg(not(windows))]
        {
            unsafe {
                libc::madvise(
                    self.base_begin().to_mut_ptr(),
                    self.base_size,
                    libc::MADV_DONTNEED,
                );
            }
        }
        /*#[cfg(debug_assertions)]
        {
            debug_assert_ne!(self.base_size(), 0);
            let mut i = 0;
            while i < self.base_size() {
                let p = self.base_begin().offset(i).to_mut_ptr::<u64>();
                unsafe {
                    debug_assert_eq!(*p, 0);
                }
                i += 8;
            }
        }*/
    }
}
