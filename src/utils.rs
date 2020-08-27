use core::cmp::Ordering;
use core::fmt;
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    pub const fn from(val: usize) -> Address {
        Address(val)
    }

    #[inline(always)]
    pub fn region_start(self, size: usize) -> Region {
        Region::new(self, self.offset(size))
    }

    #[inline(always)]
    pub fn offset_from(self, base: Address) -> usize {
        debug_assert!(self >= base);

        self.to_usize() - base.to_usize()
    }

    #[inline(always)]
    pub const fn offset(self, offset: usize) -> Address {
        Address(self.0 + offset)
    }

    #[inline(always)]
    pub const fn sub(self, offset: usize) -> Address {
        Address(self.0 - offset)
    }

    #[inline(always)]
    pub const fn add_ptr(self, words: usize) -> Address {
        Address(self.0 + words * mem::ptr_width_usize())
    }

    #[inline(always)]
    pub const fn sub_ptr(self, words: usize) -> Address {
        Address(self.0 - words * mem::ptr_width_usize())
    }

    #[inline(always)]
    pub const fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn from_ptr<T>(ptr: *const T) -> Address {
        Address(ptr as usize)
    }

    #[inline(always)]
    pub fn to_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    #[inline(always)]
    pub fn to_mut_ptr<T>(&self) -> *mut T {
        self.0 as *const T as *mut T
    }

    #[inline(always)]
    pub const fn null() -> Address {
        Address(0)
    }

    #[inline(always)]
    pub const fn is_null(self) -> bool {
        self.0 == 0
    }

    #[inline(always)]
    pub const fn is_non_null(self) -> bool {
        self.0 != 0
    }

    #[inline(always)]
    pub fn align_page(self) -> Address {
        mem::page_align(self.to_usize()).into()
    }
    pub fn align_to(self, x: usize) -> Address {
        Self::from(mem::align_usize(self.0, x))
    }
    #[inline(always)]
    pub fn align_page_down(self) -> Address {
        Address(self.0 & !(os::page_size() - 1))
    }

    #[inline(always)]
    pub fn is_page_aligned(self) -> bool {
        mem::is_page_aligned(self.to_usize())
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.to_usize())
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.to_usize())
    }
}

impl PartialOrd for Address {
    fn partial_cmp(&self, other: &Address) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Address {
    fn cmp(&self, other: &Address) -> Ordering {
        self.to_usize().cmp(&other.to_usize())
    }
}

impl From<usize> for Address {
    fn from(val: usize) -> Address {
        Address(val)
    }
}

#[derive(Copy, Clone)]
pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    pub fn new(start: Address, end: Address) -> Region {
        debug_assert!(start <= end);

        Region { start, end }
    }

    #[inline(always)]
    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    pub fn valid_top(&self, addr: Address) -> bool {
        self.start <= addr && addr <= self.end
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }

    #[inline(always)]
    pub fn empty(&self) -> bool {
        self.start == self.end
    }

    #[inline(always)]
    pub fn disjunct(&self, other: &Region) -> bool {
        self.end <= other.start || self.start >= other.end
    }

    #[inline(always)]
    pub fn overlaps(&self, other: &Region) -> bool {
        !self.disjunct(other)
    }

    #[inline(always)]
    pub fn fully_contains(&self, other: &Region) -> bool {
        self.contains(other.start) && self.valid_top(other.end)
    }
}

impl Default for Region {
    fn default() -> Region {
        Region {
            start: Address::null(),
            end: Address::null(),
        }
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub struct FormattedSize {
    pub size: usize,
}

impl fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}

#[derive(Copy, Clone)]
pub struct Slot(Address);

impl Slot {
    pub fn at(addr: Address) -> Slot {
        Slot(addr)
    }

    pub fn address(self) -> Address {
        self.0
    }

    pub fn get(self) -> Address {
        unsafe { *self.0.to_ptr::<Address>() }
    }

    pub fn set(self, obj: Address) {
        unsafe {
            *self.0.to_mut_ptr::<Address>() = obj;
        }
    }

    pub fn new<T>(p: *const T) -> Self {
        Self(Address::from_ptr(p))
    }
}

pub const fn round_down(x: usize, n: usize) -> usize {
    (x as isize & -(n as isize)) as usize
}

pub const fn round_up(x: usize, n: usize) -> usize {
    round_down(x + n - 1, n)
}
pub mod mem {
    use super::os;
    use core::i32;
    use core::mem::size_of;

    /// return pointer width: either 4 or 8
    #[inline(always)]
    pub const fn ptr_width() -> i32 {
        size_of::<*const u8>() as i32
    }

    #[inline(always)]
    pub const fn ptr_width_usize() -> usize {
        size_of::<*const u8>() as usize
    }

    /// returns true if given value is a multiple of a page size.
    pub fn is_page_aligned(val: usize) -> bool {
        let align = os::page_size_bits();

        // we can use shifts here since we know that
        // page size is power of 2
        val == ((val >> align) << align)
    }

    #[test]
    fn test_is_page_aligned() {
        let p = os::page_size();

        assert_eq!(false, is_page_aligned(1));
        assert_eq!(false, is_page_aligned(2));
        assert_eq!(false, is_page_aligned(64));
        assert_eq!(true, is_page_aligned(p));
        assert_eq!(true, is_page_aligned(2 * p));
        assert_eq!(true, is_page_aligned(3 * p));
    }

    /// round the given value up to the nearest multiple of a page
    pub fn page_align(val: usize) -> usize {
        let align = os::page_size_bits();

        // we know that page size is power of 2, hence
        // we can use shifts instead of expensive division
        ((val + (1 << align) - 1) >> align) << align
    }

    #[test]
    fn test_page_align() {
        let p = os::page_size();

        assert_eq!(p, page_align(1));
        assert_eq!(p, page_align(p - 1));
        assert_eq!(p, page_align(p));
        assert_eq!(2 * p, page_align(p + 1));
    }

    /// rounds the given value `val` up to the nearest multiple
    /// of `align`
    pub fn align(value: u32, align: u32) -> u32 {
        if align == 0 {
            return value;
        }

        ((value + align - 1) / align) * align
    }

    /// rounds the given value `val` up to the nearest multiple
    /// of `align`
    pub fn align_i32(value: i32, align: i32) -> i32 {
        if align == 0 {
            return value;
        }

        ((value + align - 1) / align) * align
    }

    /// rounds the given value `val` up to the nearest multiple
    /// of `align`.
    pub fn align_usize(value: usize, align: usize) -> usize {
        if align == 0 {
            return value;
        }

        ((value + align - 1) / align) * align
    }

    /// returns 'true' if th given `value` is already aligned
    /// to `align`.
    pub fn is_aligned(value: usize, align: usize) -> bool {
        align_usize(value, align) == value
    }

    /// returns true if value fits into u8 (unsigned 8bits).
    pub fn fits_u8(value: i64) -> bool {
        0 <= value && value <= 255
    }

    /// returns true if value fits into i32 (signed 32bits).
    pub fn fits_i32(value: i64) -> bool {
        i32::MIN as i64 <= value && value <= i32::MAX as i64
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_fits_u8() {
            assert_eq!(true, fits_u8(0));
            assert_eq!(true, fits_u8(255));
            assert_eq!(false, fits_u8(256));
            assert_eq!(false, fits_u8(-1));
        }

        #[test]
        fn test_fits_i32() {
            assert_eq!(true, fits_i32(0));
            assert_eq!(true, fits_i32(i32::MAX as i64));
            assert_eq!(true, fits_i32(i32::MIN as i64));
            assert_eq!(false, fits_i32(i32::MAX as i64 + 1));
            assert_eq!(false, fits_i32(i32::MIN as i64 - 1));
        }
    }
}

pub mod os {

    static mut PAGE_SIZE: usize = 0;
    static mut PAGE_SIZE_BITS: usize = 0;

    pub fn page_size() -> usize {
        let result = unsafe { PAGE_SIZE };

        if result != 0 {
            return result;
        }

        init_page_size();

        unsafe { PAGE_SIZE }
    }

    pub fn page_size_bits() -> usize {
        let result = unsafe { PAGE_SIZE_BITS };

        if result != 0 {
            return result;
        }

        init_page_size();

        unsafe { PAGE_SIZE_BITS }
    }

    fn init_page_size() {
        unsafe {
            PAGE_SIZE = determine_page_size();
            assert!((PAGE_SIZE & (PAGE_SIZE - 1)) == 0);

            PAGE_SIZE_BITS = log2(PAGE_SIZE);
        }
    }

    #[cfg(target_family = "unix")]
    fn determine_page_size() -> usize {
        let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

        if val <= 0 {
            panic!("could not determine page size.");
        }

        val as usize
    }

    #[cfg(target_family = "windows")]
    fn determine_page_size() -> usize {
        use winapi::um::sysinfoapi::{GetSystemInfo, LPSYSTEM_INFO, SYSTEM_INFO};

        unsafe {
            let mut system_info: SYSTEM_INFO = core::mem::zeroed();
            GetSystemInfo(&mut system_info as LPSYSTEM_INFO);

            system_info.dwPageSize as usize
        }
    }

    /// determine log_2 of given value
    fn log2(mut val: usize) -> usize {
        let mut log = 0;
        assert!(val <= u32::max_value() as usize);

        if (val & 0xFFFF0000) != 0 {
            val >>= 16;
            log += 16;
        }
        if val >= 256 {
            val >>= 8;
            log += 8;
        }
        if val >= 16 {
            val >>= 4;
            log += 4;
        }
        if val >= 4 {
            val >>= 2;
            log += 2;
        }

        log + (val >> 1)
    }

    #[test]
    fn test_log2() {
        for i in 0..32 {
            assert_eq!(i, log2(1 << i));
        }
    }
}
pub trait Bool {
    const RES: bool;
}

pub struct True;
pub struct False;

impl Bool for True {
    const RES: bool = true;
}
impl Bool for False {
    const RES: bool = false;
}
#[repr(C)]
pub struct Ref<T> {
    pub ptr: *mut T,
}

impl<T> Ref<T> {
    pub const fn null() -> Self {
        Self {
            ptr: core::ptr::null_mut(),
        }
    }
    pub fn uget(&self) -> &mut T {
        unsafe { &mut *self.ptr }
    }

    pub const fn new(val: *const T) -> Self {
        Self { ptr: val as *mut _ }
    }

    pub fn is_null(self) -> bool {
        self.ptr.is_null()
    }

    pub fn is_non_null(self) -> bool {
        !self.is_null()
    }

    pub fn cast<U>(self) -> Ref<U> {
        Ref {
            ptr: self.ptr.cast(),
        }
    }

    pub fn offset(&self, ix: isize) -> Self {
        Self {
            ptr: unsafe { self.ptr.offset(ix) },
        }
    }

    pub fn write(self, val: T) {
        unsafe { self.ptr.write(val) }
    }

    pub fn read(self) -> T {
        unsafe { self.ptr.read() }
    }
}
/// Cast reference to atomic type
#[macro_export]
macro_rules! as_atomic {
    ($val: expr,$t: ty) => {
        &*($val as *const _ as *const $t)
    };

    (ref $val: expr,$t: ty) => {
        unsafe { &*(&$val as *const _ as *const $t) }
    };
}
impl<T> core::ops::Deref for Ref<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.uget()
    }
}

impl<T> core::ops::DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.uget()
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<T> {}
