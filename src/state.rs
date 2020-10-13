use std::ffi::CString;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::ptr::{self, NonNull};

use crate::ffi::*;
use crate::types::LuaValue;
use crate::{LuaError, LuaFunction, LuaObject, LuaResult, ThreadStatus};

use libc::{c_int, c_void};

extern "C" fn rust_realloc(
  _: *mut c_void,
  mem: *mut c_void,
  oldsize: usize,
  newsize: usize,
) -> *mut c_void {
  use std::alloc::{alloc, dealloc, realloc, Layout};

  let new_layout = match Layout::from_size_align(newsize, 16) {
    Ok(layout) => layout,
    Err(_) => return ptr::null_mut(),
  };
  let old_layout = match Layout::from_size_align(oldsize, 16) {
    Ok(layout) => layout,
    Err(_) => return ptr::null_mut(),
  };

  if newsize == 0 {
    unsafe {
      dealloc(mem as _, old_layout);
    }
    return ptr::null_mut();
  }

  if oldsize == 0 {
    return unsafe { alloc(new_layout) } as _;
  }

  return unsafe { realloc(mem as _, old_layout, newsize) as _ };
}

pub struct OwnedState {
  state: State,
}

impl OwnedState {
  pub unsafe fn new() -> Self {
    Self::from_ptr(lua_newstate(Some(rust_realloc), ptr::null_mut()))
  }

  pub unsafe fn from_ptr(state: *mut lua_State) -> Self {
    Self {
      state: State::from_ptr(state),
    }
  }
}

impl Deref for OwnedState {
  type Target = State;

  fn deref(&self) -> &Self::Target {
    &self.state
  }
}

impl DerefMut for OwnedState {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.state
  }
}

impl Drop for OwnedState {
  fn drop(&mut self) {
    unsafe { lua_close(self.state.as_mut_ptr()) }
  }
}

pub struct State {
  state: NonNull<lua_State>,
}

impl State {
  /// Wraps an existing state object within a `State` object.
  ///
  /// # Safety
  /// The `state` pointer must be a valid state pointer and must
  /// live at least as long as the returned `State` object.
  ///
  /// # Panics
  /// Panics if `state` is null.
  pub unsafe fn from_ptr(state: *mut lua_State) -> Self {
    let state = match NonNull::new(state) {
      Some(state) => state,
      None => panic!("state pointer was null"),
    };

    Self { state }
  }

  pub fn as_ptr(&self) -> *const lua_State {
    self.state.as_ptr() as *const _
  }

  pub fn as_mut_ptr(&mut self) -> *mut lua_State {
    self.state.as_ptr()
  }
}

impl State {
  /// Opens the Lua standard library on this state.
  ///
  /// You can use the other `open_*` methods to fine tune
  /// what library functions should be available to Lua scripts.
  pub fn open_libs(&mut self) {
    unsafe {
      luaL_openlibs(self.as_mut_ptr());
    }
  }

  /// Opens the Lua basic library on this state.
  pub fn open_base(&mut self) {
    unsafe {
      luaopen_base(self.as_mut_ptr());
    }
  }

  /// Opens the Lua math library on this state.
  pub fn open_math(&mut self) {
    unsafe {
      luaopen_math(self.as_mut_ptr());
    }
  }

  /// Opens the Lua string library on this state.
  pub fn open_string(&mut self) {
    unsafe {
      luaopen_string(self.as_mut_ptr());
    }
  }

  /// Opens the Lua table library on this state.
  pub fn open_table(&mut self) {
    unsafe {
      luaopen_table(self.as_mut_ptr());
    }
  }

  /// Opens the Lua io library on this state.
  pub fn open_io(&mut self) {
    unsafe {
      luaopen_io(self.as_mut_ptr());
    }
  }

  /// Opens the Lua os library on this state.
  pub fn open_os(&mut self) {
    unsafe {
      luaopen_os(self.as_mut_ptr());
    }
  }

  /// Opens the Lua package library on this state.
  pub fn open_package(&mut self) {
    unsafe {
      luaopen_package(self.as_mut_ptr());
    }
  }

  /// Opens the Lua debug library on this state.
  pub fn open_debug(&mut self) {
    unsafe {
      luaopen_debug(self.as_mut_ptr());
    }
  }

  /// Opens the Lua bit library on this state.
  pub fn open_bit(&mut self) {
    unsafe {
      luaopen_bit(self.as_mut_ptr());
    }
  }

  /// Opens the LuaJIT JIT library on this state.
  pub fn open_jit(&mut self) {
    unsafe {
      luaopen_jit(self.as_mut_ptr());
    }
  }

  /// Opens the Lua FFI library on this state.
  pub fn open_ffi(&mut self) {
    unsafe {
      luaopen_ffi(self.as_mut_ptr());
    }
  }

  /// Sets the top of the stack to the valid index `idx`
  pub fn settop(&mut self, idx: i32) {
    crate::set_top(self, idx)
  }

  /// Pops a value from the top of the stack
  pub fn pop(&mut self, n: i32) {
    crate::pop(self, n)
  }

  /// Executes an arbitrary string as Lua code.
  ///
  /// # Examples
  ///
  /// ```
  /// use luajit::{OwnedState, State, ThreadStatus};
  ///
  /// let mut state = unsafe { OwnedState::new() }; // Create new Lua state
  /// state.open_base(); // Need to open base libraries for `print` to be available
  ///
  /// let status = state.do_string(r#"print("Hello world!")"#);
  /// assert!(status == Ok(ThreadStatus::Normal));
  /// ```
  pub fn do_string(&mut self, s: &str) -> crate::LuaResult<ThreadStatus> {
    let cstr = CString::new(s).unwrap();
    unsafe { ThreadStatus::from_ret(luaL_dostring(self.as_mut_ptr(), cstr.as_ptr())) }
  }

  /// Maps to `lua_call`, calls the function on the top of the
  /// stack.
  pub unsafe fn call(&mut self, nargs: i32, nres: i32) {
    crate::call(self, nargs, nres)
  }

  /// Maps directly to `lua_pcall` without additional handling.
  pub fn pcall(&mut self, nargs: i32, nres: i32, err_func: i32) -> LuaResult<()> {
    crate::pcall(self, nargs, nres, err_func)
  }

  /// Registers function `f` as a Lua global named `name`
  ///
  /// # Examples
  ///
  /// ```
  /// use luajit::{OwnedState, State, ThreadStatus, c_int};
  /// use luajit::ffi::lua_State;
  ///
  /// unsafe extern "C" fn hello(L: *mut lua_State) -> c_int {
  ///     println!("Hello world!");
  ///
  ///     0
  /// }
  ///
  /// let mut state = unsafe { OwnedState::new() };
  /// state.register("hello", hello);
  ///
  /// let status = state.do_string("hello()");
  /// assert!(status == Ok(ThreadStatus::Normal));
  /// ```
  ///
  /// Using an argument.
  ///
  /// ```
  /// use luajit::{OwnedState, State, ThreadStatus, c_int};
  /// use luajit::ffi::lua_State;
  ///
  /// unsafe extern "C" fn hello_name(l: *mut lua_State) -> c_int {
  ///     let mut state = State::from_ptr(l);
  ///     match state.to_str(1) {
  ///         Some(s) => println!("Hello {}", s),
  ///         None => println!("You have no name!"),
  ///     }
  ///
  ///     0
  /// }
  ///
  /// let mut state = unsafe { OwnedState::new() };
  /// state.register("hello", hello_name);
  ///
  /// let status = state.do_string(r#"hello("world!")"#);
  /// assert!(status == Ok(ThreadStatus::Normal));
  /// ```
  pub fn register(&mut self, name: &str, f: LuaFunction) {
    crate::register(self, name, f)
  }

  /// Test if the value at `idx` on the stack is a number.
  pub fn is_number(&mut self, idx: c_int) -> bool {
    crate::is_number(self, idx)
  }

  /// Test if the value at `idx` on the stack is a string.
  pub fn is_string(&mut self, idx: c_int) -> bool {
    crate::is_string(self, idx)
  }

  /// Test if the value at `idx` on the stack is a boolean.
  pub fn is_bool(&mut self, idx: c_int) -> bool {
    crate::is_boolean(self, idx)
  }

  /// Test if the value at `idx` on the stack is a userdata object
  pub fn is_userdata(&mut self, idx: c_int) -> bool {
    crate::is_userdata(self, idx)
  }

  /// Test if the value at `idx` on the stack is nil.
  pub fn is_nil(&mut self, idx: c_int) -> bool {
    crate::is_nil(self, idx)
  }

  /// Test if the value at `idx` on the stack is a table.
  pub fn is_table(&mut self, idx: c_int) -> bool {
    crate::is_table(self, idx)
  }

  /// Retrieves a string from the Lua stack.
  pub fn to_str(&mut self, idx: c_int) -> Option<&str> {
    crate::to_string(self, idx)
  }

  /// Return the value on the stack at `idx` as an integer.
  pub fn to_int(&mut self, idx: c_int) -> Option<i32> {
    if self.is_number(idx) {
      Some(crate::to_integer(self, idx) as _)
    } else {
      None
    }
  }

  /// Return the value on the stack at `idx` as an integer.
  pub fn to_long(&mut self, idx: c_int) -> Option<i64> {
    if self.is_number(idx) {
      Some(crate::to_integer(self, idx) as _)
    } else {
      None
    }
  }

  /// Return the value on the stack at `idx` as an bool.
  pub fn to_bool(&mut self, idx: c_int) -> Option<bool> {
    if self.is_bool(idx) {
      Some(crate::to_boolean(self, idx))
    } else {
      None
    }
  }

  /// Return the value on the stack at `idx` as an float.
  pub fn to_float(&mut self, idx: c_int) -> Option<f32> {
    if self.is_number(idx) {
      Some(crate::to_number(self, idx) as _)
    } else {
      None
    }
  }

  /// Return the value on the stack at `idx` as an double.
  pub fn to_double(&mut self, idx: c_int) -> Option<f64> {
    if self.is_number(idx) {
      Some(crate::to_number(self, idx) as _)
    } else {
      None
    }
  }

  /// Returns the userdata on the top of the Lua stack as a raw pointer
  pub fn to_raw_userdata(&mut self, idx: c_int) -> Option<*mut c_void> {
    if self.is_userdata(idx) {
      Some(crate::to_userdata(self, idx))
    } else {
      None
    }
  }

  /// Returns the userdata from the top of the Lua stack, cast as a pointer
  /// to type `T`
  ///
  /// See [`new_userdata`](#method.new_userdata) for more usage.
  pub fn to_userdata<T>(&mut self, idx: c_int) -> Option<*mut T> {
    self.to_raw_userdata(idx).map(|pt| pt as *mut T)
  }

  /// Validates that the userdata at `idx` has metatable `ty` from the Lua
  /// registry and returns a pointer to the userdata object
  pub fn check_userdata_ex<T>(&mut self, idx: c_int, ty: &str) -> Option<*mut T> {
    unsafe {
      let name = CString::new(ty).unwrap();
      let udata = luaL_checkudata(self.as_mut_ptr(), idx, name.as_ptr());

      if udata == ptr::null_mut() {
        None
      } else {
        Some(udata as *mut T)
      }
    }
  }

  /// Validates that the userdata at `idx` is an instance of struct `T` where
  /// `T` implements `LuaObject`, and returns a pointer to the userdata object
  pub fn check_userdata<T>(&mut self, idx: i32) -> Option<*mut T>
  where
    T: LuaObject,
  {
    unsafe {
      let udata = luaL_checkudata(self.as_mut_ptr(), idx, T::name());

      if udata == ptr::null_mut() {
        None
      } else {
        Some(udata as *mut T)
      }
    }
  }

  /// Pops a value of the Lua stack and sets it as a global value
  /// named `name`
  pub fn set_global(&mut self, name: &str) {
    crate::set_global(self, name)
  }

  /// Sets the value of `name` on the table `t` pointed
  /// to by `idx` as the value on the top of the stack.
  ///
  /// Equivalent to `t[name] = v` where `t` is the value at
  /// `idx` and `v` is the value at the top of the stack
  pub fn set_field(&mut self, idx: i32, name: &str) {
    crate::set_field(self, idx, name)
  }

  /// Registers all functions in `fns` on the global table `name`. If name
  /// is `None`, all functions are instead registered on the value on the top
  /// of the stack.
  pub fn register_fns(&mut self, name: Option<&str>, mut fns: Vec<luaL_Reg>) {
    // Add a sentinel struct, even if one already exists adding a second
    // shouldn't break anything and incur minimal overhead
    fns.push(luaL_Reg {
      name: ptr::null(),
      func: None,
    });

    match name {
      Some(s) => unsafe {
        luaL_register(
          self.as_mut_ptr(),
          CString::new(s).unwrap().as_ptr(),
          fns.as_ptr(),
        );
      },
      None => unsafe {
        luaL_register(self.as_mut_ptr(), ptr::null(), fns.as_ptr());
      },
    }
  }

  /// Copys the value at `idx` to the top of the stack
  pub fn push_value(&mut self, idx: i32) {
    self.checkstack(1);
    crate::push_value(self, idx)
  }

  /// Pushes a LuaValue to the lua stack.
  ///
  /// # Examples
  ///
  /// ```
  /// use luajit::OwnedState;
  ///
  /// let mut state = unsafe { OwnedState::new() };
  /// state.push(5);
  /// state.push("Hello world!");
  /// ```
  ///
  /// Can also be used with structs that implement `LuaObject`
  ///
  /// ```
  /// #[macro_use] extern crate luajit;
  ///
  /// use luajit::{OwnedState, State, LuaObject, c_int};
  /// use luajit::ffi::luaL_Reg;
  ///
  /// struct Point2D {
  ///     x: i32,
  ///     y: i32,
  /// }
  ///
  /// impl LuaObject for Point2D {
  ///     fn name() -> *const i8 {
  ///         c_str!("Point2D")
  ///     }
  ///
  ///     fn lua_fns() -> Vec<luaL_Reg> {
  ///         vec!(lua_method!("add", Point2D, Point2D::add))
  ///     }
  /// }
  ///
  /// impl Point2D {
  ///     fn add(&mut self, state: &mut State) -> c_int {
  ///         state.push(self.x + self.y);
  ///
  ///         1
  ///     }
  ///
  ///     fn new() -> Point2D {
  ///         Point2D {
  ///             x: 0,
  ///             y: 0,
  ///         }
  ///     }
  /// }
  ///
  ///
  /// fn main() {
  ///     let mut state = unsafe { OwnedState::new() };
  ///     state.open_libs();
  ///     state.push(Point2D::new());
  ///     state.set_global("point");
  ///     let res = state.do_string(r#"print(point:add())"#);
  ///     assert_eq!(res, Ok(luajit::ThreadStatus::Normal));
  /// }
  /// ```
  pub fn push<T>(&mut self, val: T)
  where
    T: LuaValue,
  {
    val.push_val(self.as_mut_ptr());
  }

  /// Push a new nil value onto the Lua stack.
  pub fn push_nil(&mut self) {
    self.checkstack(1);
    unsafe {
      lua_pushnil(self.as_mut_ptr());
    }
  }

  /// Gets a value from the globals object and pushes it to the
  /// top of the stack.
  pub fn get_global(&mut self, name: &str) {
    self.checkstack(1);
    crate::get_global(self, name)
  }

  /// Gets a value `name` from the table on the stack at `idx` and
  /// and pushes the fetched value to the top of the stack.
  pub fn get_field(&mut self, idx: i32, name: &str) {
    self.checkstack(1);
    crate::get_field(self, idx, name)
  }

  /// Creates a new table and pushes it to the top of the stack
  pub fn new_table(&mut self) {
    self.checkstack(1);
    crate::new_table(self)
  }

  /// Allocates a new Lua userdata block, and returns the pointer
  /// to it. The returned pointer is owned by the Lua state.
  pub fn new_raw_userdata(&mut self, sz: usize) -> *mut c_void {
    self.checkstack(1);
    unsafe {
      let new_ptr = lua_newuserdata(self.as_mut_ptr(), sz);
      if new_ptr == ptr::null_mut() {
        panic!("Lua returned null pointer allocating new userdata");
      }

      new_ptr
    }
  }

  /// Allocates a new Lua userdata block of size `sizeof(T)` for
  /// use to store Rust objects on the Lua stack. The returned
  /// pointer is owned by the Lua state.
  ///
  /// # Examples
  ///
  /// Useful for pushing an arbitrary struct to the Lua stack
  ///
  /// ```
  /// extern crate luajit;
  ///
  /// use luajit::{State, OwnedState};
  ///
  /// struct Point2D {
  ///     x: i32,
  ///     y: i32,
  /// }
  ///
  /// impl Point2D {
  ///     fn add(&self) -> i32 {
  ///         self.x + self.y
  ///     }
  ///     
  ///     fn set_x(&mut self, x: i32) {
  ///         self.x = x;
  ///     }
  ///
  ///     fn set_y(&mut self, y: i32) {
  ///         self.y = y;
  ///     }
  ///
  ///     fn new() -> Point2D {
  ///         Point2D {
  ///             x: 0,
  ///             y: 0,
  ///         }
  ///     }
  /// }
  ///
  ///
  /// fn main() {
  ///     let mut state = unsafe{ OwnedState::new() };
  ///     state.open_libs();
  ///     unsafe {
  ///         *state.new_userdata() = Point2D::new();
  ///     }
  ///
  ///     let point: &mut Point2D = unsafe { &mut *state.to_userdata(-1).unwrap() };
  ///     
  ///     point.set_x(2);
  ///     point.set_y(4);
  ///
  ///     assert_eq!(point.add(), 6);
  /// }
  /// ```
  pub fn new_userdata<T>(&mut self) -> *mut T {
    crate::new_userdata::<T>(self)
  }

  /// Registers all of the methods for LuaObject `T` as a global metatable
  /// with name `struct_type` and leaves it on the top of the stack.
  pub fn register_struct<T>(&mut self)
  where
    T: LuaObject,
  {
    unsafe {
      if luaL_newmetatable(self.as_mut_ptr(), T::name()) == 1 {
        self.new_table();
        self.register_fns(None, T::lua_fns());

        self.push_value(-1);
        lua_setglobal(self.as_mut_ptr(), T::name());

        self.set_field(-2, "__index");
      }
    }
  }

  /// Allocates a userdata object on the Lua stack for storing a rust struct.
  /// This method also sets the userdata object's metatable to the metatable
  /// saved for `struct_type`, it will call `lua_fns` and create a new metatable
  /// to store in the Lua registry if one has not already been created.
  pub(crate) fn new_struct<T>(&mut self) -> *mut T
  where
    T: LuaObject,
  {
    let userdata = self.new_userdata();

    unsafe {
      self.register_struct::<T>();

      lua_setmetatable(self.as_mut_ptr(), -2);
    }

    userdata
  }

  /// Maps to `luaL_loadfile`, this method validates that the file exists
  /// before passing it into the Lua C API.
  pub fn load_file(&mut self, path: &Path) -> LuaResult<()> {
    if path.is_file() {
      let p = path.canonicalize().unwrap();
      let full_path = p.to_string_lossy();

      unsafe {
        let cstr = CString::new(full_path.as_ref()).unwrap();
        LuaError::from_ret(luaL_loadfile(self.as_mut_ptr(), cstr.as_ptr() as *const i8))
      }
    } else {
      self.push("Path does not exist");
      Err(LuaError::File)
    }
  }

  /// Equivalent of `luaL_dofile`, loads a file and then immediately executes
  /// it with `pcall`, returning the result.
  pub fn do_file(&mut self, path: &Path) -> LuaResult<()> {
    self
      .load_file(path)
      .and_then(|_| self.pcall(0, LUA_MULTIRET, 0))
  }

  /// Ensures that there are at least `n` free stack slots in the stack. Returns
  /// false if it cannot grow the stack to that size.
  pub fn checkstack(&mut self, n: usize) -> bool {
    crate::checkstack(self, n as _)
  }
}
