//use crate::chronos::Node;
use crate::datatypes::*;
use core::cell::{Cell, UnsafeCell};
use std::{
    alloc::{alloc, dealloc, Layout},
    fmt::{Debug, Display},
    marker::PhantomData,
    mem::{replace, size_of},
    ops::Deref,
    ptr::{write, NonNull},
    slice::from_raw_parts_mut,
};

const BLOCK_SIZE_BITS: usize = 15;
const BLOCK_SIZE: usize = 1 << BLOCK_SIZE_BITS; // 2 ^ 15 = 32768

const LINE_SIZE_BITS: usize = 7;
const LINE_SIZE: usize = 1 << LINE_SIZE_BITS; // 128
const LINE_COUNT: usize = BLOCK_SIZE / LINE_SIZE; // 256

const MAX_ALLOC_SIZE: usize = std::u32::MAX as usize;

const FIRST_OBJECT_OFFSET: usize = size_of::<usize>() * 2;
const BLOCK_CAPACITY: usize = BLOCK_SIZE - FIRST_OBJECT_OFFSET;

const SMALL_OBJECT_MIN: usize = 1;
const SMALL_OBJECT_MAX: usize = LINE_SIZE;
const MEDIUM_OBJECT_MIN: usize = SMALL_OBJECT_MAX + 1;
const MEDIUM_OBJECT_MAX: usize = BLOCK_CAPACITY;
const LARGE_OBJECT_MIN: usize = MEDIUM_OBJECT_MAX + 1;
const LARGE_OBJECT_MAX: usize = MAX_ALLOC_SIZE;

#[derive(Debug, PartialEq)]
enum BlockError {
    BadRequest,
    OutOfMemory,
}

struct Block {
    ptr: NonNull<u8>,
    size: usize,
}

impl Block {
    //allocates a block of memory of given size
    fn new(size: usize) -> Result<Block, BlockError> {
        if !size.is_power_of_two() {
            Err(BlockError::BadRequest)
        } else {
            Ok(Block {
                ptr: alloc_block(size)?,
                size,
            })
        }
    }

    fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        dealloc_block(self.ptr, self.size);
    }
}

fn alloc_block(size: usize) -> Result<NonNull<u8>, BlockError> {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);
        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(BlockError::OutOfMemory)
        } else {
            Ok(NonNull::new_unchecked(ptr))
        }
    }
}

fn dealloc_block(ptr: NonNull<u8>, size: usize) {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);
        dealloc(ptr.as_ptr(), layout);
    }
}

struct BlockMeta {
    line_mark: [bool; LINE_COUNT],
    block_mark: bool,
}

impl BlockMeta {
    fn new_boxed() -> Box<BlockMeta> {
        Box::new(BlockMeta {
            line_mark: [false; LINE_COUNT],
            block_mark: false,
        })
    }

    fn mark_line(&mut self, index: usize) {
        self.line_mark[index] = true;
    }

    fn mark_block(&mut self) {
        self.block_mark = true;
    }

    fn reset(&mut self) {
        for bit in self.line_mark.iter_mut() {
            *bit = false;
        }
        self.block_mark = false;
    }

    fn line_iter(&self) -> impl Iterator<Item = &'_ bool> {
        self.line_mark.iter()
    }

    fn find_next_free_space(&self, start_index: usize) -> Option<(usize, usize)> {
        let mut count = 0;
        let mut start: Option<usize> = None;
        let mut stop: usize = 0;

        let start_line = start_index / LINE_SIZE;

        for (index, marked) in self.line_mark[start_line..].iter().enumerate() {
            let abs_index = start_line + index;

            if !*marked {
                count += 1;

                if count == 1 && abs_index > 0 {
                    continue;
                }

                if start.is_none() {
                    start = Some(abs_index);
                }

                stop = abs_index + 1;
            }

            if count > 0 && (*marked || stop >= LINE_COUNT) {
                if let Some(start) = start {
                    let cursor = start * LINE_SIZE;
                    let limit = stop * LINE_SIZE;
                    return Some((cursor, limit));
                }
            }

            if *marked {
                count = 0;
                start = None;
            }
        }
        None
    }
}

struct BumpBlock {
    cursor: usize,
    limit: usize,
    block: Block,
    meta: Box<BlockMeta>,
}

impl BumpBlock {
    fn new() -> Result<BumpBlock, AllocError> {
        let mut block = BumpBlock {
            cursor: FIRST_OBJECT_OFFSET,
            limit: BLOCK_SIZE,
            block: Block::new(BLOCK_SIZE)?,
            meta: BlockMeta::new_boxed(),
        };

        let meta_ptr: *const BlockMeta = &*block.meta;
        unsafe { block.write(meta_ptr, 0) };

        Ok(block)
    }

    unsafe fn write<T>(&mut self, object: T, offset: usize) -> *const T {
        let p = self.block.as_ptr().add(offset) as *mut T;
        write(p, object);
        p
    }

    //returns a pointer to a block of memory of size alloc_size
    fn inner_alloc(&mut self, alloc_size: usize) -> Option<*const u8> {
        let next_bump = self.cursor + alloc_size;

        if next_bump > self.limit {
            if self.limit < BLOCK_SIZE {
                if let Some((cursor, limit)) = self.meta.find_next_free_space(self.limit) {
                    self.cursor = cursor;
                    self.limit = limit;
                    return self.inner_alloc(alloc_size);
                }
            }
            None
        } else {
            let offset = self.cursor;
            self.cursor = next_bump;
            unsafe { Some(self.block.as_ptr().add(offset) as *const u8) }
        }
    }

    fn current_space_size(&self) -> usize {
        self.limit - self.cursor
    }
}

enum AllocError {
    BadRequest,
    OutOfMemory,
}

impl From<BlockError> for AllocError {
    fn from(error: BlockError) -> AllocError {
        match error {
            BlockError::BadRequest => AllocError::BadRequest,
            BlockError::OutOfMemory => AllocError::OutOfMemory,
        }
    }
}

struct BlockList {
    head: Option<BumpBlock>,
    overflow: Option<BumpBlock>,
    rest: Vec<BumpBlock>,
}

impl BlockList {
    fn new() -> BlockList {
        BlockList {
            head: None,
            overflow: None,
            rest: Vec::new(),
        }
    }

    fn overflow_alloc(&mut self, alloc_size: usize) -> Result<*const u8, AllocError> {
        Ok(match self.overflow {
            Some(ref mut overflow) => match overflow.inner_alloc(alloc_size) {
                Some(space) => space,
                None => {
                    let previous = replace(overflow, BumpBlock::new()?);
                    self.rest.push(previous);
                    overflow
                        .inner_alloc(alloc_size)
                        .expect("This should fit in an empty block")
                }
            },
            None => {
                let mut overflow = BumpBlock::new()?;
                let space = overflow
                    .inner_alloc(alloc_size)
                    .expect("This should fit in an empty block");
                self.overflow = Some(overflow);
                space
            }
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum SizeClass {
    Small,  //fits inside a line
    Medium, // fits inside a block
    Large,  // more than one block
}

impl SizeClass {
    fn get_size(size: usize) -> Result<SizeClass, AllocError> {
        match size {
            SMALL_OBJECT_MIN..=SMALL_OBJECT_MAX => Ok(SizeClass::Small),
            MEDIUM_OBJECT_MIN..=MEDIUM_OBJECT_MAX => Ok(SizeClass::Medium),
            LARGE_OBJECT_MIN..=LARGE_OBJECT_MAX => Ok(SizeClass::Large),
            _ => Err(AllocError::BadRequest),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Mark {
    Allocated,
    Unmarked,
    Marked,
}

struct RawPtr<T: Sized> {
    ptr: NonNull<T>,
}

impl<T> RawPtr<T> {
    fn new(ptr: *const T) -> RawPtr<T> {
        RawPtr {
            ptr: unsafe { NonNull::new_unchecked(ptr as *mut T) },
        }
    }

    fn as_ptr(self) -> *const T {
        self.ptr.as_ptr()
    }

    fn as_word(self) -> usize {
        self.ptr.as_ptr() as usize
    }

    fn as_typed<U>(self) -> NonNull<U> {
        self.ptr.cast()
    }

    unsafe fn as_ref(&self) -> &T {
        self.ptr.as_ref()
    }

    unsafe fn as_mut_ref(&mut self) -> &mut T {
        self.ptr.as_mut()
    }
}

impl<T> Clone for RawPtr<T> {
    fn clone(&self) -> Self {
        RawPtr { ptr: self.ptr }
    }

    fn clone_from(&mut self, source: &Self) {
        self.ptr = source.ptr;
    }
}

impl<T> Copy for RawPtr<T> {}

impl<T> PartialEq for RawPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

//implemented by heap
trait AllocRaw {
    type Header: AllocHeader;

    fn alloc_array(&self, size: u32) -> Result<RawPtr<u8>, AllocError>;
    fn alloc<T>(&self, object: T) -> Result<RawPtr<T>, AllocError>
    where
        T: AllocObject<<Self::Header as AllocHeader>::TypeId>;

    fn get_header(object: NonNull<()>) -> NonNull<Self::Header>;
    fn get_object(header: NonNull<Self::Header>) -> NonNull<()>;
}

//trait AllocTypeId: Copy + Clone {}

//implemented by objects
trait AllocObject<T: Copy + Clone> {
    const TYPE_ID: T;
}

//implemented by headers
trait AllocHeader: Sized {
    type TypeId: Copy + Clone;

    fn new<O: AllocObject<Self::TypeId>>(size: u32, size_class: SizeClass, mark: Mark) -> Self;
    fn new_array(size: u32, size_class: SizeClass, mark: Mark) -> Self;
    fn mark(&mut self);
    fn is_marked(&self) -> bool;
    fn size_class(&self) -> SizeClass;
    fn size(&self) -> u32;
    fn type_id(&self) -> Self::TypeId;
}

struct ImmixHeap<H: AllocHeader> {
    blocks: UnsafeCell<BlockList>,
    _header_type: PhantomData<*const H>,
}

impl<H: AllocHeader> ImmixHeap<H> {
    pub fn new() -> ImmixHeap<H> {
        ImmixHeap {
            blocks: UnsafeCell::new(BlockList::new()),
            _header_type: PhantomData,
        }
    }

    fn find_space(
        &self,
        alloc_size: usize,
        size_class: SizeClass,
    ) -> Result<*const u8, AllocError> {
        let blocks = unsafe { &mut *self.blocks.get() };

        if size_class == SizeClass::Large {
            return Err(AllocError::BadRequest);
        }

        let space = match blocks.head {
            Some(ref mut head) => {
                if size_class == SizeClass::Medium && alloc_size > head.current_space_size() {
                    return blocks.overflow_alloc(alloc_size);
                }

                match head.inner_alloc(alloc_size) {
                    Some(space) => space,

                    None => {
                        let previous = replace(head, BumpBlock::new()?);
                        blocks.rest.push(previous);
                        head.inner_alloc(alloc_size).unwrap()
                    }
                }
            }

            None => {
                let mut head = BumpBlock::new()?;

                let space = head.inner_alloc(alloc_size).unwrap();
                blocks.head = Some(head);
                space
            }
        } as *const u8;

        Ok(space)
    }

    unsafe fn write_to_memory<T>(space: &*const u8, object: T) {
        write(*space as *mut T, object);
    }
}

impl<H: AllocHeader> AllocRaw for ImmixHeap<H> {
    type Header = H;

    fn alloc<T>(&self, object: T) -> Result<RawPtr<T>, AllocError>
    where
        T: AllocObject<<Self::Header as AllocHeader>::TypeId>,
    {
        let header_size = size_of::<Self::Header>();
        let object_size = size_of::<T>();

        let alloc_size = alloc_size_of(header_size + object_size);
        let size_class = SizeClass::get_size(alloc_size)?;

        let space = self.find_space(alloc_size, size_class)?;
        let header = Self::Header::new::<T>(object_size as u32, size_class, Mark::Allocated);

        unsafe {
            Self::write_to_memory(&space, header);
        }
        //unsafe {
        //    write(space as *mut Self::Header, header);
        //}

        let object_space = unsafe { space.add(header_size) };
        unsafe {
            Self::write_to_memory(&object_space, object);
        }
        //unsafe {
        //    write(object_space as *mut T, object);
        //}

        Ok(RawPtr::new(object_space as *const T))
    }

    fn alloc_array(&self, size: u32) -> Result<RawPtr<u8>, AllocError> {
        let header_size = size_of::<Self::Header>();

        let alloc_size = alloc_size_of(header_size + size as usize);
        let size_class = SizeClass::get_size(alloc_size)?;
        let space = self.find_space(alloc_size, size_class)?;

        let header = Self::Header::new_array(size, size_class, Mark::Allocated);
        unsafe {
            Self::write_to_memory(&space, header);
        }
        //unsafe {
        //    write(space as *mut Self::Header, header);
        //}

        let object_space = unsafe { space.add(header_size) };
        let array: &mut [u8] =
            unsafe { from_raw_parts_mut(object_space as *mut u8, size as usize) };

        for byte in array {
            *byte = 0;
        }

        Ok(RawPtr::new(object_space as *const u8))
    }

    fn get_header(object: NonNull<()>) -> NonNull<Self::Header> {
        unsafe { NonNull::new_unchecked(object.cast::<Self::Header>().as_ptr().offset(-1)) }
    }

    fn get_object(header: NonNull<Self::Header>) -> NonNull<()> {
        unsafe { NonNull::new_unchecked(header.as_ptr().offset(1).cast::<()>()) }
    }
}

//rounds up to the next multiple of align
//align maybe should be 16 for objects greater then 64
//not sure if this is correct
fn alloc_size_of(object_size: usize) -> usize {
    let align = size_of::<usize>(); // should this be align_of::<T>()?
    (object_size + (align - 1)) & !(align - 1)
    //object_size = 2;
    //object_size = object_size - 1;
    //object_size |= object_size >> 1;
    //object_size |= object_size >> 2;
    //object_size |= object_size >> 4;
    //object_size |= object_size >> 8;
    //object_size |= object_size >> 16;
    //return object_size + 1;
}

trait ScopedLifeTime {}

struct ScopedPtr<'a, T: Sized> {
    ptr: &'a T,
}

impl<'a, T> ScopedPtr<'a, T> {
    fn new(value: &'a T, _guard: &'a dyn ScopedLifeTime) -> ScopedPtr<'a, T> {
        ScopedPtr { ptr: value }
    }
}

impl<'a, T> ScopedLifeTime for ScopedPtr<'a, T> {}

impl<'a, T> Clone for ScopedPtr<'a, T> {
    fn clone(&self) -> ScopedPtr<'a, T> {
        ScopedPtr { ptr: self.ptr }
    }
}

impl<'a, T> Copy for ScopedPtr<'a, T> {}

impl<'a, T> Deref for ScopedPtr<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.ptr
    }
}

impl<'a, T: Sized + Display> Display for ScopedPtr<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ptr)
    }
}

impl<'a, T: Sized + Debug> Debug for ScopedPtr<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ptr)
    }
}

impl<'a, T: Sized + PartialEq> PartialEq for ScopedPtr<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

trait HasScopedRef<T> {
    //_guard ensures that is is safe to dereference
    fn get_scoped_ref<'a>(&self, _guard: &'a dyn ScopedLifeTime) -> &'a T;
}

impl<T> HasScopedRef<T> for RawPtr<T> {
    fn get_scoped_ref<'a>(&self, _guard: &'a dyn ScopedLifeTime) -> &'a T {
        unsafe { &*self.as_ptr() }
    }
}

#[derive(Clone)]
pub struct CellPtr<T: Sized> {
    ptr: Cell<RawPtr<T>>,
}

impl<T> CellPtr<T> {

    fn from_scoped_ptr(source: ScopedPtr<T>) -> CellPtr<T> {
        CellPtr {
            ptr: Cell::new(RawPtr::new(source.ptr)),
        }
    }

    fn get_scoped_ptr<'a>(&self, guard: &'a dyn ScopedLifeTime) -> ScopedPtr<'a, T> {
        ScopedPtr::new(self.ptr.get().get_scoped_ref(guard), guard)
    }

    fn set(&self, source: ScopedPtr<T>) {
        self.ptr.set(RawPtr::new(source.ptr))
    }
}

#[derive(Clone, Copy, PartialEq)]
enum TypeList {
    UnTypedArray,
    Number,
}

struct ObjectHeader {
    mark: Mark,
    size_class: SizeClass,
    type_id: TypeList,
    size: u32,
}

impl AllocHeader for ObjectHeader {
    type TypeId = TypeList;

    fn type_id(&self) -> Self::TypeId {
        self.type_id
    }

    fn size(&self) -> u32 {
        self.size
    }

    fn size_class(&self) -> SizeClass {
        self.size_class
    }

    fn is_marked(&self) -> bool {
        self.mark == Mark::Marked
    }

    fn mark(&mut self) {
        self.mark = Mark::Marked;
    }

    fn new<O: AllocObject<Self::TypeId>>(size: u32, size_class: SizeClass, mark: Mark) -> Self {
        ObjectHeader {
            mark,
            size_class,
            type_id: O::TYPE_ID,
            size,
        }
    }

    fn new_array(size: u32, size_class: SizeClass, mark: Mark) -> Self {
        ObjectHeader {
            mark,
            size_class,
            type_id: TypeList::UnTypedArray,
            size: size as u32,
        }
    }
}

struct Heap {
    heap: ImmixHeap<ObjectHeader>,
    //symbol_map: SymbolMap,
}

struct Memory {
    heap: Heap,
}

impl Memory
{
    fn mutate<M: Mutator>(&self, mutator: &M, input: M::Input) -> Result<M::Output, RuntimeError> {
        let mut guard = MutatorView::new(self);
        mutator.run(&mut guard, input)
    }
}

enum RuntimeError {
    BadRequest,
    OutOfMemory,
}

impl From<AllocError> for RuntimeError {
    fn from(error: AllocError) -> RuntimeError {
        match error {
            AllocError::BadRequest => RuntimeError::BadRequest,
            AllocError::OutOfMemory => RuntimeError::OutOfMemory,
        }
    }
}

impl Heap {
    fn alloc<T: AllocObject<TypeList>>(&self, object: T) -> Result<RawPtr<T>, RuntimeError> {
        Ok(self.heap.alloc(object)?)
    }
}

struct MutatorView<'a> {
    heap: &'a Heap,
}

impl<'a> ScopedLifeTime for MutatorView<'a> {}

impl<'a> MutatorView<'a> {
    fn new(memory: &'a Memory) -> MutatorView<'a> {
        MutatorView { heap: &memory.heap }
    }

    fn alloc<T: AllocObject<TypeList>>(&self, object: T) -> Result<ScopedPtr<'_, T>, RuntimeError> {
        Ok(ScopedPtr::new(
            self.heap.alloc(object)?.get_scoped_ref(self),
            self,
        ))
    }
}

trait Mutator: Sized {
    type Input;
    type Output;

    fn run(&self, mem: &MutatorView, input: Self::Input) -> Result<Self::Output, RuntimeError>;
}

#[derive(Clone, Copy)]
pub union TaggedPtr {
    tag: usize,
    object: NonNull<()>,
}
