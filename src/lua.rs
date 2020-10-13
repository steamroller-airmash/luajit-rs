use std::ffi::{CStr, CString};
use std::ptr;

use crate::ffi::*;
use crate::{LuaFunction, OwnedState, State};

use libc::{c_int, c_void};

pub type AllocFn = lua_Alloc;
/// Type for C functions.
pub type CFunction = lua_CFunction;

pub type LuaResult<T> = Result<T, LuaError>;

pub const MULTIRET: c_int = LUA_MULTIRET;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum LuaError {
  /// A runtime error
  Runtime = LUA_ERRRUN as isize,
  /// A memory allocation error.
  ///
  /// For such errors, Lua does not call the error handler function.
  Memory = LUA_ERRMEM as isize,
  /// An error occurred while running the error handler function.
  Error = LUA_ERRERR as isize,
  Syntax = LUA_ERRSYNTAX as isize,
  File = LUA_ERRFILE as isize,
  Unknown,
}

impl LuaError {
  pub(crate) fn from_ret(ret: c_int) -> LuaResult<()> {
    Err(match ret {
      LUA_OK => return Ok(()),
      LUA_ERRRUN => Self::Runtime,
      LUA_ERRMEM => Self::Memory,
      LUA_ERRERR => Self::Error,
      LUA_ERRSYNTAX => Self::Syntax,
      LUA_ERRFILE => Self::File,
      _ => Self::Unknown,
    })
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ThreadStatus {
  Normal = LUA_OK as isize,
  Yielded = LUA_YIELD as isize,
}

impl ThreadStatus {
  pub(crate) fn from_ret(ret: c_int) -> LuaResult<Self> {
    match ret {
      LUA_OK => Ok(Self::Normal),
      LUA_YIELD => Ok(Self::Yielded),
      _ => LuaError::from_ret(ret).map(|_| unreachable!()),
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Gc {
  Stop = LUA_GCSTOP as isize,
  Restart = LUA_GCRESTART as isize,
  Collect = LUA_GCCOLLECT as isize,
  Count = LUA_GCCOUNT as isize,
  CountB = LUA_GCCOUNTB as isize,
  Step = LUA_GCSTEP as isize,
  SetPause = LUA_GCSETPAUSE as isize,
  SetStepMul = LUA_GCSETSTEPMUL as isize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LuaType {
  None = LUA_TNONE as isize,
  Nil = LUA_TNIL as isize,
  Number = LUA_TNUMBER as isize,
  Boolean = LUA_TBOOLEAN as isize,
  String = LUA_TSTRING as isize,
  Table = LUA_TTABLE as isize,
  Function = LUA_TFUNCTION as isize,
  UserData = LUA_TUSERDATA as isize,
  Thread = LUA_TTHREAD as isize,
  LightUserData = LUA_TLIGHTUSERDATA as isize,
}

impl LuaType {
  pub(crate) fn from_ret(ret: c_int) -> Option<Self> {
    Some(match ret {
      LUA_TNONE => Self::None,
      LUA_TNIL => Self::Nil,
      LUA_TNUMBER => Self::Number,
      LUA_TBOOLEAN => Self::Boolean,
      LUA_TSTRING => Self::String,
      LUA_TTABLE => Self::Table,
      LUA_TFUNCTION => Self::Function,
      LUA_TUSERDATA => Self::UserData,
      LUA_TTHREAD => Self::Thread,
      LUA_TLIGHTUSERDATA => Self::LightUserData,
      _ => return None,
    })
  }
}

/// Sets a new panic function and returns the old one.
///
/// If an error happens outside any prodected environment, Lua calls a
/// *panic function* and then calls `exit(EXIT_FAILURE)`, this exiting
/// the host application. Your panic function can avoid this exit by never
/// returning (e.g. by doing a long jump).
///
/// The panic function can access the error message at the top of the stack.
pub fn atpanic(state: &mut State, panicf: CFunction) -> CFunction {
  unsafe { lua_atpanic(state.as_mut_ptr(), panicf) }
}

/// Calls a function.
///
/// # Safety
/// This function can long jump. The safety invariants around long jump
/// are rather complicated and not quite well defined. Roughly, it is UB
/// to long jump over any rust stack frames that contain values that would
/// call destructors.
///
/// Use [`pcall`](crate::lua::pcall) to safely call a LUA function.
///
/// # Usage
/// To call a function you must use the following protocol: first, the
/// function to be called is pushed onto the stack; then, the arguments
/// to the function are pushed in direct order; that is, the first argument
/// is pushed first. Finally you call [`call`](crate::lua::call);
/// `nargs` is the number of arguments that you pushed onto the stack. All
/// arguments and the function value are popped from the stack when the
/// function is called. The function results are pushed onto the stack when
/// the function returns. The number of results is adjusted to `nresults`,
/// unless nresults is [`MULTIRET`](crate::lua::MULTIRET). In this case,
/// all results from the function are pushed. Lua takes care that the
/// returned values fit into the stack space. The function results are
/// pushed onto the stack in direct order (the first result is pushed
/// first), so that after the call the last result is on the top of the
/// stack.
///
/// Any error inside the called function is propagated upwards (with a
/// `longjmp`).
pub unsafe fn call(state: &mut State, nargs: c_int, nresults: c_int) {
  lua_call(state.as_mut_ptr(), nargs, nresults)
}

/// Ensures that there are at least `extra` free stack slots on the stack.
///
/// It returns `false` if it cannot grow the stack to that size. This
/// function never shrinks the stack; if the stack is already larger than
/// the new size, it is left unchanged.
pub fn checkstack(state: &mut State, extra: c_int) -> bool {
  unsafe { lua_checkstack(state.as_mut_ptr(), extra) != 0 }
}

/// Concatenates the n values at the top of the stack, pops them, and
/// leaves the result at the top.
///
/// If n is 1, the result is the single value on the stack (that is,
/// the function does nothing); if n is 0, the result is the empty string.
/// Concatenation is performed following the usual semantics of Lua.
pub fn concat(state: &mut State, n: c_int) {
  unsafe { lua_concat(state.as_mut_ptr(), n) }
}

/// Calls the C function `func` in protected mode.
///
/// `func` starts with only one element in its stack, a light userdata
/// containing `ud`. In case of errors, `cpcall` returns the same error codes as
/// `pcall`, plus the error object on the top of the stack; otherwise, it
/// returns zero, and does not change the stack. All values returned by `func`
/// are discarded.
///
/// # Safety
/// The userdata pointer will be stored within `state`. It must not be
/// dereferenced once the reference's lifetime has ended.
pub unsafe fn cpcall<T>(state: &mut State, func: LuaFunction, ud: &mut T) -> LuaResult<()> {
  LuaError::from_ret(lua_cpcall(
    state.as_mut_ptr(),
    Some(func),
    ud as *mut _ as *mut c_void,
  ))
}

/// Creates a new table and pushes it onto the stack.
///
/// The new table has space pre-allocated for `narr` array elements and `nrec`
/// non-array elements. This pre-allocation is useful for when you know exactly
/// how many elements the table will have. Otherwise you can use the function
/// [`newtable`](crate::lua::newtable).
pub fn create_table(state: &mut State, narr: c_int, nrec: c_int) {
  unsafe { lua_createtable(state.as_mut_ptr(), narr, nrec) }
}

// TODO
// pub fn dump

/// Compares two values on the stack.
///
/// Returns `true` if the two values in acceptable indices `index1` and `index2`
/// are equal, following the smantics of the lua `==` operator (that is, may
/// call metamethods). Otherwise returns `false`. Also returns `false` if any of
/// the indices is not valid.
pub fn equal(state: &mut State, idx1: c_int, idx2: c_int) -> bool {
  unsafe { lua_equal(state.as_mut_ptr(), idx1, idx2) != 0 }
}

/// Generate a Lua error.
///
/// The error message (which can actually be a Lua value of any type) must be on
/// the stack top. This function does a long jump, and therefore never returns.
///
/// # Safety
/// This function can long jump. The safety invariants around long jump
/// are rather complicated and not quite well defined. Roughly, it is UB
/// to long jump over any rust stack frames that contain values that would
/// call destructors.
///
/// Use [`pcall`](crate::lua::pcall) to safely call a LUA function.
pub unsafe fn error(state: &mut State) -> ! {
  lua_error(state.as_mut_ptr());
  core::hint::unreachable_unchecked()
}

/// Controls the garbage collector.
///
/// This function performs several tasks, according to the value of the paramter
/// `what`:
/// - `Gc::Stop`: stops the garbage collector.
/// - `Gc::Restart`: restarts the garbage collector.
/// - `Gc::Collect`: performs a full garbage collection cycle.
/// - `Gc::Count`: returns the current amount of memory (in KB) in use by Lua.
/// - `Gc::CountB`: return the remainder of dividing the abount of bytes of
///   memory in use by Lua by 1024.
/// - `Gc::Step`: performs an incremental step of garbage collection. The step
///   "size" is controlled by `data` (larger values mean more steps) in a
///   non-specified way. If you want to control the step size you must
///   experimentally tune the value of `data`. The function returns 1 if the
///   step finished a garbage collection cycle.
/// - `Gc::SetPause`: sets `data` as the new value for the *pause* of the
///   collector. The function returns the previous value of tha pause.
/// - `Gc::SetStepMul`: sets `data` as the new value for the *step multiplier*
///   of the collector. The function returns the previous value of the step
///   multiplier.
pub fn gc(state: &mut State, what: Gc, data: c_int) -> c_int {
  unsafe { lua_gc(state.as_mut_ptr(), what as _, data) }
}

// TODO: Change reference to `lua_newstate` to proper allocation function.
/// Get the memory allocation function of a given state.
///
/// If `ud` is not `None`, Lua stores in `*ud` the opaque pointer passed
/// to `lua_newstate`.
pub fn get_allocf(state: &mut State, ud: Option<&mut *mut c_void>) -> AllocFn {
  unsafe {
    lua_getallocf(
      state.as_mut_ptr(),
      ud.map(|x| x as *mut _).unwrap_or(ptr::null_mut()),
    )
  }
}

/// Push the environment table of the value at the given index onto the stack.
pub fn get_fenv(state: &mut State, index: c_int) {
  unsafe { lua_getfenv(state.as_mut_ptr(), index) }
}

/// Pushes the field for the table at `index` on the the stack.
///
/// This pushes `t[key]` onto the stack, where `t` is the value at the
/// given valid index. As in Lua, this function may trigger a metamethod
/// for the "index" event.
///
/// # Panics
/// This function panics if `key` contains a nul byte.
pub fn get_field(state: &mut State, index: c_int, key: &str) {
  let key = CString::new(key).expect("key value contained a nul byte");
  unsafe { lua_getfield(state.as_mut_ptr(), index, key.as_ptr()) }
}

/// Pushes the value of the global `name` onto the stack.
///
/// # Panics
/// This function panics if `name` contains a nul byte.
pub fn get_global(state: &mut State, name: &str) {
  get_field(state, LUA_GLOBALSINDEX, name)
}

/// Pushes the metatable of the value at the given stack index onto the stack.
///
/// If the index is not valid, or if the value does not have a metatable, the
/// function returns 0 and pushes nothing on the stack.
pub fn get_metatable(state: &mut State, index: c_int) -> c_int {
  unsafe { lua_getmetatable(state.as_mut_ptr(), index) }
}

/// Pushes the value of `t[k]` onto the stack.
///
/// `t` is the value at the given index and `k` is the valud at the top of the
/// stack. This function pops the key from the stack (putting the resulting
/// value in its place). As in Lua, this function may trigger a metamethod for
/// the "index" event.
pub fn get_table(state: &mut State, index: c_int) {
  unsafe { lua_gettable(state.as_mut_ptr(), index) }
}

/// Returns the index of the top element in the stack.
///
/// Because indices start at 1, this result is equal to the number of elements
/// in the stack (and so 0 means an empty stack).
pub fn get_top(state: &mut State) -> c_int {
  unsafe { lua_gettop(state.as_mut_ptr()) }
}

/// Moves the top element into the given valid index.
///
/// Shifts the elements above this index to open space. Cannot be called with a
/// pseudo-index, because a pseudo-index is not an actual stack position.
pub fn insert(state: &mut State, index: c_int) {
  unsafe { lua_insert(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index has type boolean.
pub fn is_boolean(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isboolean(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index is a C function.
pub fn is_cfunction(state: &mut State, index: c_int) -> bool {
  unsafe { lua_iscfunction(state.as_mut_ptr(), index) != 0 }
}

/// Returns whether the value at the given index is a function (either C or
/// Lua).
pub fn is_function(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isfunction(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index is a light userdata.
pub fn is_lightuserdata(state: &mut State, index: c_int) -> bool {
  unsafe { lua_islightuserdata(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index is **nil**.
pub fn is_nil(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isnil(state.as_mut_ptr(), index) }
}

/// Returns whether `index` is not valid.
///
/// That is to say, it refers to an element outside the current stack.
pub fn is_none(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isnone(state.as_mut_ptr(), index) }
}

/// Returns whether `index` is invalid or nil.
pub fn is_none_or_nil(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isnoneornil(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index is a number or a string
/// convertible to a number.
pub fn is_number(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isnumber(state.as_mut_ptr(), index) != 0 }
}

/// Returns whether the value at the given index is a string or a number (which
/// is always convertible to a string).
pub fn is_string(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isstring(state.as_mut_ptr(), index) != 0 }
}

/// Returns whether the value at the given index is a table.
pub fn is_table(state: &mut State, index: c_int) -> bool {
  unsafe { lua_istable(state.as_mut_ptr(), index) }
}

/// Returns whether the value at the given index is a userdata (either full or
/// light).
pub fn is_userdata(state: &mut State, index: c_int) -> bool {
  unsafe { lua_isuserdata(state.as_mut_ptr(), index) != 0 }
}

/// Compare two values with Lua's `<` operator.
///
/// Returns whether the value at `idx1` is smaller than the value at `idx2`,
/// following the semantics of the Lua `<` operator (that is, may call
/// metamethods). Also returns `false` if any of the indices are not valid.
pub fn lessthan(state: &mut State, idx1: c_int, idx2: c_int) -> bool {
  unsafe { lua_lessthan(state.as_mut_ptr(), idx1, idx2) != 0 }
}

// TODO: Load
// pub fn load

/// Creates a new empty table and pushes it onto the stack.
///
/// Equivalent to `create_table(state, 0, 0)`.
pub fn new_table(state: &mut State) {
  unsafe { lua_newtable(state.as_mut_ptr()) }
}

/// Creates a new thread, pushes it on the stack, and returns an
/// [`OwnedState`](crate::lua::OwnedState) that represents the new thread.
///
/// The new state returned by this function shares with the original state all
/// global objects (such as tables), but has an independant execution stack.
///
/// There is no function to close or destroy a thread. Threads are subject to
/// garbage collection, like any Lua object.
pub fn new_thread(state: &mut State) -> OwnedState {
  unsafe { OwnedState::from_ptr(lua_newthread(state.as_mut_ptr())) }
}

/// Allocates a new user data, pushes it onto the stack, and returns the
/// address.
///
/// Userdata represent C values in Lua. A *full userdata* represents a block of
/// memory. It is an object (like a table): you must create it, it can have its
/// own metatable, and you can detect when it is being collected. A full
/// userdata is only equal to itself (under raw equality).
///
/// When Lua collects a full userdata with a `gc` metamethod, Lua calls the
/// metamethod and marks the userdata as finalized. When this userdata is
/// collected again then Lua frees its corresponding memory.
pub fn new_userdata<T>(state: &mut State) -> *mut T {
  assert!(std::mem::align_of::<T>() <= 16);

  unsafe { lua_newuserdata(state.as_mut_ptr(), std::mem::size_of::<T>()) as _ }
}

/// Pushes the next key-value pair after the current top of the stack.
///
/// In detail, pops a key from the stack, and pushes a key-value pair from the
/// table at the given index (the "next" pair after the given key). If there are
/// no more elements in the table then `next` returns 0 (and pushes nothing).
pub fn next(state: &mut State, index: c_int) -> c_int {
  unsafe { lua_next(state.as_mut_ptr(), index) }
}

/// Returns the "length" of the value at the given index.
///
/// For strings, this is the string length, for tables, this is the result of
/// the lengh operator (`#`); for userdata, this is the size of the block of
/// memory allocated for the userdata; for other values, it is 0.
pub fn objlen(state: &mut State, index: c_int) -> usize {
  unsafe { lua_objlen(state.as_mut_ptr(), index) }
}

/// Calls a function in protected mode.
///
/// Both `nargs` and `nresults` have the same meaning as in [`call`]. If there
/// are no errors during the call, `pcall` behaves exactly like [`call`].
/// However, if there is any error, `pcall` catches it, pushes a single value on
/// the stack (the error message), and returns an error code. Like [`call`],
/// `pcall` always removes the function and its arguments from the stack.
///
/// If `errfunc` is 0, then the error message returned on the stack is exactly
/// the original error message. Otherwise, `errfunc` is the stack index of an
/// *error handling function*. (In the current implementation, this index cannot
/// be a pseudo-index.) In case of runtime errors, this funciton will be called
/// with the error message and its return value will be the message returned on
/// the stack by `pcall`.
///
/// Typically, the error handler fnuction is used to add more debug information
/// to the error message, such as a stack traceback. Such information cannot be
/// gathered after the return of `pcall`, since by the the stack has unwound.
///
/// [`call`]: crate::lua::call
pub fn pcall(state: &mut State, nargs: c_int, nresults: c_int, errfunc: c_int) -> LuaResult<()> {
  LuaError::from_ret(unsafe { lua_pcall(state.as_mut_ptr(), nargs, nresults, errfunc) })
}

/// Pops `n` elements from the stack.
pub fn pop(state: &mut State, n: c_int) {
  unsafe { lua_pop(state.as_mut_ptr(), n) }
}

/// Push a boolean with value `b` onto the stack.
pub fn push_boolean(state: &mut State, b: bool) {
  unsafe { lua_pushboolean(state.as_mut_ptr(), if b { 1 } else { 0 }) }
}

/// Push a new C closure onto the stack.
///
/// When a C function is created, it is possible to associate some values with
/// it, thus creating a C closure; these values are then accessible to the
/// function whenever it is called. To associate values with a C function, first
/// these values should be pushed onto the stack (where there are multiple
/// values, the first value is pushed first). Then `push_cclosure` is called to
/// create and push the C function onto the stack, with the argument `n` telling
/// how many values should be associated with the function. `push_cclosure` also
/// pops these values from the stack.
///
/// The maximum value for `n` is 255.
pub fn push_cclosure(state: &mut State, fun: LuaFunction, n: c_int) {
  unsafe { lua_pushcclosure(state.as_mut_ptr(), Some(fun), n) }
}

/// Push a C function onto the stack.
///
/// This function receives a pointer to a C function and pushes onto the stack a
/// Lua value of type `function` that, when called, invokes the corresponding C
/// function.
///
/// Any function to be registered in Lua must follow the correct protocol to
/// receive its parameters and return its results.
pub fn push_cfunction(state: &mut State, fun: LuaFunction) {
  unsafe { lua_pushcfunction(state.as_mut_ptr(), Some(fun)) }
}

// TODO: fn pushfstring

/// Push a number with value `n` onto the stack.
pub fn push_integer(state: &mut State, n: lua_Integer) {
  unsafe { lua_pushinteger(state.as_mut_ptr(), n) }
}

/// Push a light userdata onto the stack.
///
/// Userdata represent C values in Lua. A *light userdata* represents a pointer.
/// It is a value (like a number): you do not create it, it has no individual
/// metatable, and it is not collected (as it was never created). A light
/// userdata is equal to "any" light userdata with the same C address.
pub fn push_lightuserdata(state: &mut State, p: *mut c_void) {
  unsafe { lua_pushlightuserdata(state.as_mut_ptr(), p) }
}

/// Equivalent to [`pushlstring`](crate::lua::push_lstring)
pub fn push_literal(state: &mut State, s: &'static str) {
  unsafe { lua_pushliteral(state.as_mut_ptr(), s) }
}

/// Pushes the string `s` onto the stack.
///
/// Lua makes (or reuses) an internal copy of the given string so the memory at
/// `s` can be freed or reused immediately after the function returns. The
/// string can contain embedded zeros.
pub fn push_string(state: &mut State, s: &str) {
  unsafe { lua_pushlstring(state.as_mut_ptr(), s.as_ptr() as *const _, s.len()) }
}

/// Push a C string onto the stack.
///
/// Pushes the zero-terminated string `s` onto the stack. Lua makes (or reuses)
/// an internal copy of the given string, so the memory at `s` can be freed or
/// reused immediately after the function returns. The string cannot contain
/// embedded zeros; it is assumed to end at the first zero.
pub fn push_cstring(state: &mut State, s: &CStr) {
  unsafe { lua_pushstring(state.as_mut_ptr(), s.as_ptr()) }
}

/// Push a byte buffer onto the stack.
pub fn push_bytes(state: &mut State, bytes: &[u8]) {
  unsafe { lua_pushlstring(state.as_mut_ptr(), bytes.as_ptr() as *const _, bytes.len()) }
}

/// Push a nil value onto the stack.
pub fn push_nil(state: &mut State) {
  unsafe { lua_pushnil(state.as_mut_ptr()) }
}

/// Push a number with value `n` onto the stack.
pub fn push_number(state: &mut State, n: lua_Number) {
  unsafe { lua_pushnumber(state.as_mut_ptr(), n) }
}

/// Pushes the thread represented by `state` onto the stack.
///
/// Returns 1 if this thread is the main thread of its state.
pub fn push_thread(state: &mut State) -> c_int {
  unsafe { lua_pushthread(state.as_mut_ptr()) }
}

/// Pushes a copy of the element at the given index onto the stack.
pub fn push_value(state: &mut State, index: c_int) {
  unsafe { lua_pushvalue(state.as_mut_ptr(), index) }
}

/// Compare the values at `idx1` and `idx2`.
///
/// Returns whether the values at `idx1` and `idx2` are primitively equal (that
/// is, without calling metamethods). Also returns false if either of the
/// indices are invalid.
pub fn rawequal(state: &mut State, idx1: c_int, idx2: c_int) -> bool {
  unsafe { lua_rawequal(state.as_mut_ptr(), idx1, idx2) != 0 }
}

/// Similar to [`get_table`](crate::lua::get_table), but does a raw access (i.e.
/// without metamethods).
pub fn rawget(state: &mut State, index: c_int) {
  unsafe { lua_rawget(state.as_mut_ptr(), index) }
}

/// Raw table array access.
///
/// Pushes `t[n]` onto the stack, where `t` is the value at the given index. The
/// access is raw; that is, it does not invoke metamethods.
pub fn rawgeti(state: &mut State, index: c_int, n: c_int) {
  unsafe { lua_rawgeti(state.as_mut_ptr(), index, n) }
}

/// Similar to [`set_table`](crate::lua::set_table), but does a raw assignment
/// (i.e. without metamethods).
pub fn rawset(state: &mut State, index: c_int) {
  unsafe { lua_rawset(state.as_mut_ptr(), index) }
}

/// Set a table index.
///
/// Does the equivalent of `t[n] = v`, where `t` is the value at the given valid
/// index and `v` is the value at the top of the stack.
///
/// This function pops the value from the stack. The assignment is raw; that is,
/// it does not invoke metamethods.
pub fn rawseti(state: &mut State, index: c_int, n: c_int) {
  unsafe { lua_rawseti(state.as_mut_ptr(), index, n) }
}

/// Sets the C function `f` as the new value of global `name`.
pub fn register(state: &mut State, name: &str, func: LuaFunction) {
  push_cfunction(state, func);
  set_global(state, name);
}

/// Removes the element at the given index.
///
/// Shifts down the elements above this index to fill the gab. Cannot be called
/// with a pseudo-index, because a pseudo-index is not an actual stack position.
pub fn remove(state: &mut State, index: c_int) {
  unsafe { lua_remove(state.as_mut_ptr(), index) }
}

/// Replaces the element at `index` with the top of the stack.
///
/// More specifically, moves the top element into the given position (and pops
/// it), without shifting any element (therefore replacing the value at the
/// given position).
pub fn replace(state: &mut State, index: c_int) {
  unsafe { lua_replace(state.as_mut_ptr(), index) }
}

/// Starts and resumes a coroutine in a given thread.
///
/// To start a coroutine, you first create a new thread (see [`new_thread`]);
/// then you push onto its stack the main function plus an arguments; then you
/// call `resume`, with `narg` being the number of arguments. This call returns
/// when the coroutine suspends or finishes its execution. When it returns, the
/// stack contains all values passed to [`yield`] or all values returned from
/// the body function. `resume` returns `ThreadStatus::Yielded` if the coroutine
/// yields, `ThreadStatus::Normal` if the coroutine finishes its execution
/// without errors, or an error in case of errors. In case of errors, the stack
/// is not unwound, so you can use the debug API over it. The error message is
/// on the top of the stack. To restart a coroutine, you put on its stack only
/// the values to be passed as results from yield, and then call `resume`.
///
/// [`new_thread`](crate::lua::new_thread)
/// [`yield`](crate::lua::yield)
pub fn resume(state: &mut State, narg: c_int) -> LuaResult<ThreadStatus> {
  ThreadStatus::from_ret(unsafe { lua_resume(state.as_mut_ptr(), narg) })
}

fn is_layout_compatible<T, U>() -> bool {
  use std::alloc::Layout;

  let tlayout = Layout::new::<T>();
  let ulayout = Layout::new::<U>();

  tlayout.size() <= ulayout.size() && tlayout.align() <= ulayout.align()
}

unsafe extern "C" fn allocf_wrapper<F>(
  ud: *mut c_void,
  ptr: *mut c_void,
  osize: usize,
  nsize: usize,
) -> *mut c_void
where
  F: Copy + Fn(*mut c_void, usize, usize) -> *mut c_void,
{
  // For types which are layout-compatible with a void pointer we avoid making an
  // allocation and instead store the user data within the provided pointer.
  let func: &F = if is_layout_compatible::<F, *mut c_void>() {
    &*(&ud as *const _ as *const F)
  } else {
    &*(ud as *const F)
  };

  func(ptr, osize, nsize)
}

/// Changes the allocator function of a given state to `func`.
pub fn set_allocf<F>(state: &mut State, func: F)
where
  F: Copy + Fn(*mut c_void, usize, usize) -> *mut c_void,
{
  let ud: *mut F = if is_layout_compatible::<F, *mut c_void>() {
    let mut ptr = std::ptr::null_mut();
    unsafe { ptr::write(&mut ptr as *mut _ as *mut F, func) }
    ptr
  } else {
    Box::into_raw(Box::new(func))
  };

  unsafe { lua_setallocf(state.as_mut_ptr(), Some(allocf_wrapper::<F>), ud as _) }
}

/// Set the environment value.
///
/// More specifically, pops a table from the stack and sets it as the new
/// environment value at the given index. If the value at the given index is
/// neither a function nor a thread nor a userdata, `set_fenv` return 0.
/// Otherwise it returns 1.
pub fn set_fenv(state: &mut State, index: c_int) -> c_int {
  unsafe { lua_setfenv(state.as_mut_ptr(), index) }
}

/// Assign to a table key.
///
/// Does the equivalent of `t[k] = v`, where `t` is the value at the given index
/// and `v` is the value at the top of the stack.
///
/// This function pops the value from the stack. As in Lua, this function may
/// trigger a metamethod for the "newindex" event.
///
/// # Panics
/// This function panics if `name` contains a nul byte.
pub fn set_field(state: &mut State, index: c_int, k: &str) {
  let key = CString::new(k).expect("key value contained a nul byte");
  unsafe { lua_setfield(state.as_mut_ptr(), index, key.as_ptr()) }
}

/// Pops a value from the stack and sets it as the new value of global `name`.
pub fn set_global(state: &mut State, name: &str) {
  let name = CString::new(name).expect("name value contained a nul byte");
  unsafe { lua_setglobal(state.as_mut_ptr(), name.as_ptr()) }
}

/// Pops a table from the stack and sets it as the new metatable for the given
/// index.
pub fn set_metatable(state: &mut State, index: c_int) -> c_int {
  unsafe { lua_setmetatable(state.as_mut_ptr(), index) }
}

/// Assign to a table.
///
/// Does the equivalent of `t[k] = v`, where `t` is the value at the given
/// index, `v` is the value at the top of the stack, and `k` is the value just
/// below the top.
///
/// This function pops both the key and the value from the stack. As in Lua,
/// this function may trigger a metamethod for the "newindex" event.
pub fn set_table(state: &mut State, index: c_int) {
  unsafe { lua_settable(state.as_mut_ptr(), index) }
}

/// Sets the stack top to the given index.
///
/// If the new top is larger than the old one, then the new elements are filled
/// with **nil**. If `index` is 0, then all stack elements are removed.
pub fn set_top(state: &mut State, index: c_int) {
  unsafe { lua_settop(state.as_mut_ptr(), index) }
}

/// Returns the status of thread `state`.
///
/// The status can be `ThreadStatus::Normal` for a normal thread, an error code
/// if the thread finished its execution with an error, or
/// `ThreadStatus::Yielded` if the thread is suspended.
pub fn status(state: &mut State) -> LuaResult<ThreadStatus> {
  ThreadStatus::from_ret(unsafe { lua_status(state.as_mut_ptr()) })
}

/// Converts the Lua value at the given index to a boolean value.
///
/// Like all tests in Lua, `to_boolean` returns true for any Lua value different
/// from false and nil; otherwise returns false. It also returns false when
/// called with a non-valid index. (If you want to accept only actual boolean
/// values, use [`is_boolean`](crate::lua::is_boolean) to test the value's
/// type.)
pub fn to_boolean(state: &mut State, index: c_int) -> bool {
  unsafe { lua_toboolean(state.as_mut_ptr(), index) != 0 }
}

/// Converts the Lua value at the given index to a C function.
///
/// The value must be a C function; otherwise, returns `None`.
pub fn to_cfunction(state: &mut State, index: c_int) -> lua_CFunction {
  unsafe { lua_tocfunction(state.as_mut_ptr(), index) }
}

/// Converts the Lua value at the given index to an integer.
///
/// The Lua value must be a number or a string convertible to a number;
/// otherwise, `to_integer` returns 0.
///
/// If the number is not an integer, is truncated in some non-specified way.
pub fn to_integer(state: &mut State, index: c_int) -> lua_Integer {
  unsafe { lua_tointeger(state.as_mut_ptr(), index) }
}

/// Converts the Lua value at the given index to a string.
///
/// The Lua value must be a string or a number; otherwise, the function returns
/// `None`. If the value is a number, then `to_bytes` also *changes the actual
/// value in the stack to a string*. (This change confuses
/// [`next`](crate::lua::next) when `to_bytes` is applied to keys during a
/// table traversal.)
///
/// `to_bytes` returns a fully aligned pointer to a byte slice inside the Lua
/// state. This slice always has a zero (`\0`) after the last character (as in
/// C) although this is not part of the returned slice. It can contain other
/// zeros in its body. Because Lua has garbage collection, there is no guarantee
/// that the pointer returned by `to_bytes` will be valid after the
/// corresponding value is removed from the stack.
///
/// Note that with these bindings it is not possible to do this without either
/// unsafely creating a second state, or using the raw ffi methods with
/// pointers.
pub fn to_bytes(state: &mut State, index: c_int) -> Option<&[u8]> {
  let mut len: usize = 0;
  unsafe {
    let ptr = lua_tolstring(state.as_mut_ptr(), index, &mut len);

    if ptr == ptr::null_mut() {
      return None;
    }

    Some(std::slice::from_raw_parts(ptr as _, len))
  }
}

/// Converts the Lua value at the given index to a string.
///
/// The Lua value must be a string or a number; otherwise, the function returns
/// `None`. If the value is a number, then `to_string` also *changes the actual
/// value in the stack to a string*. (This change confuses
/// [`next`](crate::lua::next) when `to_string` is applied to keys during a
/// table traversal.)
///
/// `to_string` returns a fully aligned pointer to a byte slice inside the Lua
/// state. This slice always has a zero (`\0`) after the last character (as in
/// C) although this is not part of the returned slice. It can contain other
/// zeros in its body. Because Lua has garbage collection, there is no guarantee
/// that the pointer returned by `to_string` will be valid after the
/// corresponding value is removed from the stack.
///
/// Note that with these bindings it is not possible to do this without either
/// unsafely creating a second state, or using the raw ffi methods with
/// pointers.
pub fn to_string(state: &mut State, index: c_int) -> Option<&str> {
  to_bytes(state, index).and_then(|bytes| std::str::from_utf8(bytes).ok())
}

/// Converts the Lua value at the given index to a number.
///
/// The Lua value must be a number of a string convertible to a number;
/// otherwise, `to_number` returns 0.
pub fn to_number(state: &mut State, index: c_int) -> lua_Number {
  unsafe { lua_tonumber(state.as_mut_ptr(), index) }
}

/// Converts the value at the given index to a generic pointer.
///
/// The value can be a userdata, a table, a thread, or a function; otherwise,
/// `to_pointer` returns null. Different objects will give different pointers.
/// There is no way to convert the pointer back to its original value.
///
/// Typically this function is only used for debug information.
pub fn to_pointer(state: &mut State, index: c_int) -> *const c_void {
  unsafe { lua_topointer(state.as_mut_ptr(), index) }
}

/// Converts the value at the given index to a Lua thread.
///
/// If this value is not a thread, returns `None`.
///
/// # Safety
/// This function gives an additional reference to an existing state. Using it
/// to pop values which are currently being referenced by rust code off the
/// stack will cause UB.
pub unsafe fn to_thread(state: &mut State, index: c_int) -> Option<State> {
  let other = lua_tothread(state.as_mut_ptr(), index);

  if other == ptr::null_mut() {
    None
  } else {
    Some(State::from_ptr(other))
  }
}

/// Get the address to the userdata at the given index.
///
/// If the value at the given acceptable index is a full userdata, returns its
/// block address. If the value is a light userdata, returns its pointer.
/// Otherwise, returns null.
pub fn to_userdata(state: &mut State, index: c_int) -> *mut c_void {
  unsafe { lua_touserdata(state.as_mut_ptr(), index) }
}

/// Returns the type of the value in the given index.
///
/// If the index is invalid, then returns `LuaType::None`.
pub fn type_of(state: &mut State, index: c_int) -> LuaType {
  LuaType::from_ret(unsafe { lua_type(state.as_mut_ptr(), index) }).unwrap_or(LuaType::None)
}

/// Returns the name of the type `tp`.
pub fn type_name(state: &mut State, tp: LuaType) -> &'static str {
  unsafe {
    let name = lua_typename(state.as_mut_ptr(), tp as _);
    let cname = CStr::from_ptr(name);
    cname.to_str().expect("Type name contained invalid UTF-8")
  }
}

/// Exchange values between different threads of the *same* global state.
///
/// This function pops `n` values from the stack `from`, and pushes them onto
/// the stack `to`.
pub fn xmove(from: &mut State, to: &mut State, n: c_int) {
  unsafe { lua_xmove(from.as_mut_ptr(), to.as_mut_ptr(), n) }
}

/// Yields a coroutine.
///
/// This function should only be called as the return expression of a function,
/// as follows:
///
/// ```ignore
/// return unsafe { yield(state, nresults) };
/// ```
///
/// When a function calls `yield` in that way, the running coroutine suspends
/// its execution, and the call to [`resume`] that started this coroutine
/// returns. The parameter `nresults` is the number of values from the stack
/// that are passed as results to [`resume`].
///
/// # Safety
/// It is UB to call this function other than as the entire part of a return
/// statement.
///
/// This function can long jump. The safety invariants around long jump
/// are rather complicated and not quite well defined. Roughly, it is UB
/// to long jump over any rust stack frames that contain values that would
/// call destructors.
///
/// [`resume`]: crate::lua::resume
pub unsafe fn lua_yield(state: &mut State, nresults: c_int) -> c_int {
  crate::ffi::lua_yield(state.as_mut_ptr(), nresults)
}
