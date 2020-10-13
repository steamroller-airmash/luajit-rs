//! Bindings to Lua's Debug interface.

use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt;

use crate::ffi::*;
use crate::{c_int, State};

#[derive(Copy, Clone, Debug)]
pub enum DebugEvent {
  Call = LUA_HOOKCALL as isize,
  Ret = LUA_HOOKRET as isize,
  Line = LUA_HOOKLINE as isize,
  Count = LUA_HOOKCOUNT as isize,
  TailRet = LUA_HOOKTAILRET as isize,
}

impl TryFrom<c_int> for DebugEvent {
  type Error = std::num::TryFromIntError;

  fn try_from(event: c_int) -> Result<Self, Self::Error> {
    Ok(match event {
      LUA_HOOKCALL => Self::Call,
      LUA_HOOKRET => Self::Ret,
      LUA_HOOKTAILRET => Self::TailRet,
      LUA_HOOKLINE => Self::Line,
      LUA_HOOKCOUNT => Self::Count,
      _ => {
        u8::try_from(256u16)?;
        unreachable!()
      }
    })
  }
}

#[repr(transparent)]
pub struct Debug {
  debug: lua_Debug,
}

impl Debug {
  pub fn new() -> Self {
    unsafe { std::mem::zeroed() }
  }

  pub unsafe fn from_ptr<'a>(debug: *const lua_Debug) -> &'a Self {
    &*(debug as *const Self)
  }
  pub unsafe fn from_ptr_mut<'a>(debug: *mut lua_Debug) -> &'a mut Self {
    &mut *(debug as *mut Self)
  }

  #[inline]
  pub fn as_ptr(&self) -> *const lua_Debug {
    &self.debug
  }

  #[inline]
  pub fn as_mut_ptr(&mut self) -> *mut lua_Debug {
    &mut self.debug
  }

  /// If the function was defined in a string, then `source` is that string. If
  /// the function was defined in a file, then `source` starts with an `@`
  /// followed by the file name.
  ///
  /// Note that this will also be none in the case where `source` contains
  /// invalid UTF-8. Use `c_source` in that case.
  #[inline]
  pub fn source(&self) -> Option<&str> {
    self.c_source().and_then(|x| x.to_str().ok())
  }

  /// If the function was defined in a string, then `source` is that string. If
  /// the function was defined in a file, then `source` starts with an `@`
  /// followed by the file name.
  #[inline]
  pub fn c_source(&self) -> Option<&CStr> {
    if self.debug.source.is_null() {
      return None;
    }

    unsafe { Some(CStr::from_ptr(self.debug.source)) }
  }

  /// A "printable" version of `source`, to be used in error messages.
  ///
  /// # Panics
  /// Panics if `short_src` contains invalid UTF-8.
  #[inline]
  pub fn short_src(&self) -> &str {
    self
      .c_short_src()
      .to_str()
      .expect("short_src contained invalid UTF-8")
  }

  /// A "printable" version of `source`, to be used in error messages.
  #[inline]
  pub fn c_short_src(&self) -> &CStr {
    unsafe { CStr::from_ptr(self.debug.short_src.as_ptr()) }
  }

  /// The line number where the definition of the function starts.
  #[inline]
  pub fn line_defined(&self) -> u32 {
    self.debug.linedefined as _
  }

  /// The line number where the definition of the function ends.
  #[inline]
  pub fn last_line_defined(&self) -> u32 {
    self.debug.lastlinedefined as _
  }

  /// The string "Lua" if the function is a Lua function, "C" if it is a C
  /// function, "main" if it is the main part of a chunk, and "tail" if it was a
  /// function that did a tail call. In the latter case, Lua has no other
  /// information about the function.
  #[inline]
  pub fn what(&self) -> Option<&str> {
    if self.debug.what.is_null() {
      return None;
    }

    Some(unsafe {
      CStr::from_ptr(self.debug.what)
        .to_str()
        .expect("what contained invalid UTF-8")
    })
  }

  /// The current line where the given function is executing. When no line
  /// information is available this is `None`.
  #[inline]
  pub fn current_line(&self) -> Option<u32> {
    self.debug.currentline.try_into().ok()
  }

  /// A reasonable name for the given function. Because functions in Lua are
  /// first-class values, they do not have a fixed name: some functions can be
  /// the value of multiple global variables, while others can be stored only in
  /// a table field. The [`get_info`](crate::debug::get_info) function checks
  /// how the function was called to find a suitable name. If it cannot find a
  /// name, then `name` is `None`.
  ///
  /// Note that if `name` contains invalid UTF-8 then this also returns `None`.
  #[inline]
  pub fn name(&self) -> Option<&str> {
    self.c_name().and_then(|x| x.to_str().ok())
  }

  /// A reasonable name for the given function. Because functions in Lua are
  /// first-class values, they do not have a fixed name: some functions can be
  /// the value of multiple global variables, while others can be stored only in
  /// a table field. The [`get_info`](crate::debug::get_info) function checks
  /// how the function was called to find a suitable name. If it cannot find a
  /// name, then `name` is `None`.
  #[inline]
  pub fn c_name(&self) -> Option<&CStr> {
    if self.debug.name.is_null() {
      return None;
    }

    unsafe { Some(CStr::from_ptr(self.debug.name)) }
  }

  /// Explains the `name` field. The value of `name_what` can be `"global"`,
  /// `"local"`, `"method"`, `"field"`, `"upvalue"`, or `""` (the empty string),
  /// according to how the function was called. (Lua uses the empty string when
  /// no other option seems to apply.)
  #[inline]
  pub fn name_what(&self) -> Option<&str> {
    if self.debug.namewhat.is_null() {
      return None;
    }

    Some(unsafe {
      CStr::from_ptr(self.debug.namewhat)
        .to_str()
        .expect("name_what contained invalid UTF-8")
    })
  }

  /// The number of upvalues of the function.
  pub fn nups(&self) -> u32 {
    self.debug.nups as _
  }

  /// Get the event type for a HookFn.
  ///
  /// # Panics
  /// Panics if the event type is not one of the known event types.
  pub fn event(&self) -> DebugEvent {
    self
      .debug
      .event
      .try_into()
      .expect("Unexpected debug event type")
  }
}

impl Default for Debug {
  fn default() -> Self {
    Self::new()
  }
}

pub struct DebugError(());

impl fmt::Debug for DebugError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_tuple("DebugError")
      .field(&"get_info returned error")
      .finish()
  }
}

impl fmt::Display for DebugError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("get_info returned error")
  }
}

impl Error for DebugError {}

/// Return information about a specific function or function invocation.
///
/// To get information about a function invocation, the parameter `ar` must be a
/// valid activation record that was filled by a previous call to [`get_stack`]
/// or given as an argument to a hook.
///
/// To get information about a function you push it onto the stack and start the
/// `what` string with the character `>`. (In that case, `get_info` pops the
/// function on the top of the stack.) For instance, to know in which line a
/// function `f` was defined, you can write the following code
///
/// ```rust
/// # use luajit::{debug::*, OwnedState};
/// # let mut state = unsafe { OwnedState::new() };
/// let mut ar = Debug::new();
/// luajit::get_global(&mut state, "f"); // get global 'f'
/// luajit::debug::get_info(&mut state, ">S", &mut ar);
/// println!("{}", ar.line_defined());
/// ```
///
/// Each character in the string `what` selects some fields of the structure
/// `ar` to be filled or a value to be pushed on the stack:
/// - `n`: fills in the fields `name` and `name_what`.
/// - `S`: fills in the fields `source`, `short_src`, `line_defined`,
///   `last_line_defined`, and `what`.
/// - `l`: fills in the field `current_line`.
/// - `u`: fills in the field `nups`.
/// - `f`: pushes onto the stack the function that is running at the given
///   level.
/// - `L`: pushes onto the stack a table whose indices are the numbers of the
///   lines that are valid on the function. (A *valid line* is a line with some
///   associated code, that is, a line where you can put a break point.
///   invalid lines include empty lines and comments.)
pub fn get_info(state: &mut State, what: &str, ar: &mut Debug) -> Result<(), DebugError> {
  let what = CString::new(what).expect("what string contained a nul byte");
  let ret = unsafe { lua_getinfo(state.as_mut_ptr(), what.as_ptr(), ar.as_mut_ptr()) };

  match ret {
    0 => Err(DebugError(())),
    _ => Ok(()),
  }
}

/// Gets information about a local variable of a given activation record.
///
/// The parameter `ar` must be avalid activation record that was filled by a
/// previous call to `get_stack` or given as an argument to a hook. The index
/// `n` selects which local variable to inspect (1 is the first parameter or
/// active local variable, and so on, until the last active local variable).
/// `get_local` pushes the variable's value onto the stack and returns its name.
///
/// Variable names starting with `(` (open parentheses) represent internal
/// variables (loop control variables, temporaries, and C function locals).
///
/// Returns `None` (and pushes nothing) when the index is greater than the
/// number of active local variables.
pub fn get_local_c<'s>(state: &'s mut State, ar: &mut Debug, n: c_int) -> Option<&'s CStr> {
  unsafe {
    let name = lua_getlocal(state.as_mut_ptr(), ar.as_mut_ptr(), n);

    if name.is_null() {
      return None;
    }

    Some(CStr::from_ptr(name))
  }
}

/// Gets information about a local variable of a given activation record.
///
/// The parameter `ar` must be avalid activation record that was filled by a
/// previous call to `get_stack` or given as an argument to a hook. The index
/// `n` selects which local variable to inspect (1 is the first parameter or
/// active local variable, and so on, until the last active local variable).
/// `get_local` pushes the variable's value onto the stack and returns its name.
///
/// Variable names starting with `(` (open parentheses) represent internal
/// variables (loop control variables, temporaries, and C function locals).
///
/// Returns `None` (and pushes nothing) when the index is greater than the
/// number of active local variables.
///
/// # Panics
/// Panics if the returned name contains invalid UTF-8. In the case where a
/// panic occurs, this method will pop the returned value off the stack.
pub fn get_local<'s>(state: &'s mut State, ar: &mut Debug, n: c_int) -> Option<&'s str> {
  // Need to work around the borrow checker here
  let dup = unsafe { State::from_ptr(state.as_mut_ptr()) };

  let e = match get_local_c(state, ar, n).map(CStr::to_str) {
    Some(Ok(name)) => return Some(name),
    Some(Err(e)) => e,
    None => return None,
  };

  dup.pop(1);
  panic!("local name contained invalid UTF-8: {}", e);
}

/// Get information about the interpreter runtime stack.
///
/// This function returns a `Debug` structure with an identification of the
/// *activation record* of the function executing at a given level. Level 0 is
/// the current running function, whereas level *n + 1* is the function that has
/// called level *n*. If an error occurs, `get_stack` returns `None`.
pub fn get_stack(state: &mut State, level: c_int) -> Option<Debug> {
  let mut debug = Debug::new();

  unsafe {
    match lua_getstack(state.as_mut_ptr(), level, debug.as_mut_ptr()) {
      0 => Some(debug),
      _ => None,
    }
  }
}

/// Get information about a closure's upvalue.
///
/// (For Lua functions, upvalues are the external local variables that the
/// function uses, and that are consequently included in its closure.)
/// `get_upvalue` gets the index `n` of an upvalue, pushes the upvalue's value
/// onto the stack, and returns its name. `funcindex` points to the closure in
/// the stack. (Upvalues have no particular order, as they are active through
/// the whole function. So they are numbered in arbitrary order.)
///
/// Returns `None` (and pushes nothing) when the index is greater than the
/// number of upvalues. For C functions, this function uses the empty string
/// `""` as a name for all upvalues.
pub fn get_upvalue_c(state: &mut State, funcindex: c_int, n: c_int) -> Option<&CStr> {
  unsafe {
    let name = lua_getupvalue(state.as_mut_ptr(), funcindex, n);

    if name.is_null() {
      return None;
    }

    Some(CStr::from_ptr(name))
  }
}
/// Get information about a closure's upvalue.
///
/// (For Lua functions, upvalues are the external local variables that the
/// function uses, and that are consequently included in its closure.)
/// `get_upvalue` gets the index `n` of an upvalue, pushes the upvalue's value
/// onto the stack, and returns its name. `funcindex` points to the closure in
/// the stack. (Upvalues have no particular order, as they are active through
/// the whole function. So they are numbered in arbitrary order.)
///
/// Returns `None` (and pushes nothing) when the index is greater than the
/// number of upvalues. For C functions, this function uses the empty string
/// `""` as a name for all upvalues.
///
/// # Panics
/// Panics if the returned name contains invalid UTF-8. In the case where a
/// panic occurs, this method will pop the returned value off the stack.
pub fn get_upvalue<'a>(state: &'a mut State, funcindex: c_int, n: c_int) -> Option<&'a str> {
  // Need to work around the borrow checker here
  let dup = unsafe { State::from_ptr(state.as_mut_ptr()) };

  let e = match get_upvalue_c(state, funcindex, n).map(CStr::to_str) {
    Some(Ok(name)) => return Some(name),
    None => return None,
    Some(Err(e)) => e,
  };

  dup.pop(1);
  panic!("local name contained invalid UTF-8: {}", e);
}

/// Type for debugging hook functions.
///
/// Whenever a hook is called, its `ar` argument has its field `event` set to
/// the specific event that triggered the hook. These events correspond to the
/// `DebugEvent` variants. Moreover, for line events, the field `current_line`
/// is also set. To get the value of any other field in `ar`, the hook must call
/// [`get_info`](crate::debug::get_info). For return events, `event` can be
/// `DebugEvent::Ret`, the normal value, or `DebugEvent::TailRet`. In the latter
/// case, Lua is simulating a return from a function that did a tail call; in
/// this case it is useless to call [`get_info`](crate::debug::get_info).
///
/// While Lua is running a hook, it disables other calls to hooks. Therefore, if
/// a hook calls back to Lua to execute a function or a chunk, this execution
/// occurs without any calls to hooks.
///
/// # ABI Notes
/// This function signature is fully compatible with the C function signature
/// otherwise used.
pub type HookFn = extern "C" fn(&mut State, &mut Debug);

bitflags! {
  pub struct HookMask: c_int {
    const CALL = LUA_MASKCALL;
    const RET = LUA_MASKRET;
    const LINE = LUA_MASKLINE;
    const COUNT = LUA_MASKCOUNT;
  }
}

/// Sets the debugging hook function.
///
/// Argument `hook` is the hook function. `mask` specifies on shich events the
/// hook will be called. The `count` argument is only meaningful when the mask
/// includes `HookMask::COUNT`. For each event, the hook is called as explained
/// below:
/// - **The call hook:** is called when the interpreter calls a function. The
///   hook is called just after Lua enters the new function, before the function
///   gets its arguments.
/// - **The return hook:** is called when the interpreter returns from a
///   function. The hook is called just before Lua leaves the function. You have
///   no access to the values to be returned by the function.
/// - **The line hook:** is called when the interpreter is about to start the
///   execution of a new line of code, or when it jumps back in the code (even
///   to the same line). (This event only happens while Lua is executing a Lua
///   function.)
/// - **The count hook:** is called after the interpreter executes every `count`
///   instructions. (This event only happens while Lua is executing a Lua
///   function.)
///
/// A hook is disabled by setting `mask` to zero.
pub fn set_hook(state: &mut State, hook: HookFn, mask: HookMask, count: c_int) -> c_int {
  unsafe {
    lua_sethook(
      state.as_mut_ptr(),
      std::mem::transmute(hook),
      mask.bits(),
      count,
    )
  }
}

/// Sets the value of a local variable of a given activation record.
///
/// Parameters `ar` and `n` are as in [`get_local`]. `set_local` assigns the
/// value at the top of thestack to the variable and returns its name. It also
/// pops the value from the stack.
///
/// Returns `None` (and pops nothing) when the index is greater than the number
/// of active local variables.
///
/// [`get_local`]: crate::debug::get_local
pub fn set_local_c<'a>(state: &'a mut State, debug: &mut Debug, n: c_int) -> Option<&'a CStr> {
  unsafe {
    let name = lua_setlocal(state.as_mut_ptr(), debug.as_mut_ptr(), n);

    if name.is_null() {
      return None;
    }

    Some(CStr::from_ptr(name))
  }
}

/// Sets the value of a local variable of a given activation record.
///
/// Parameters `ar` and `n` are as in [`get_local`]. `set_local` assigns the
/// value at the top of thestack to the variable and returns its name. It also
/// pops the value from the stack.
///
/// Returns `None` (and pops nothing) when the index is greater than the number
/// of active local variables.
///
/// # Panics
/// Panics if the returned name contains invalid UTF-8. In the case where a
/// panic occurs, this method will pop the returned value off the stack.
///
/// [`get_local`]: crate::debug::get_local
pub fn set_local<'a>(state: &'a mut State, debug: &mut Debug, n: c_int) -> Option<&'a str> {
  // Need to work around the borrow checker here
  let dup = unsafe { State::from_ptr(state.as_mut_ptr()) };

  let e = match set_local_c(state, debug, n).map(CStr::to_str) {
    Some(Ok(name)) => return Some(name),
    None => return None,
    Some(Err(e)) => e,
  };

  dup.pop(1);
  panic!("local name contained invalid UTF-8: {}", e);
}

/// Sets the value of a closure's upvalue.
///
/// It assigns the value at the top of the stack to the upvalue and returns its
/// name. It also pops the value from the stack. Parameters `funcindex` and `n`
/// are as in [`get_upvalue`].
///
/// Returns `None` (and pops nothing) when the index is greater than the number
/// of upvalues.
///
/// [`get_upvalue`]: crate::debug::get_upvalue
pub fn set_upvalue_c<'a>(state: &'a mut State, funcindex: c_int, n: c_int) -> Option<&'a CStr> {
  unsafe {
    let name = lua_setupvalue(state.as_mut_ptr(), funcindex, n);

    if name.is_null() {
      return None;
    }

    Some(CStr::from_ptr(name))
  }
}

/// Sets the value of a closure's upvalue.
///
/// It assigns the value at the top of the stack to the upvalue and returns its
/// name. It also pops the value from the stack. Parameters `funcindex` and `n`
/// are as in [`get_upvalue`].
///
/// Returns `None` (and pops nothing) when the index is greater than the number
/// of upvalues.
///
/// # Panics
/// Panics if the returned name contains invalid UTF-8. In the case where a
/// panic occurs, this method will pop the returned value off the stack.
///
/// [`get_upvalue`]: crate::debug::get_upvalue
pub fn set_upvalue<'a>(state: &'a mut State, funcindex: c_int, n: c_int) -> Option<&'a str> {
  // Need to work around the borrow checker here
  let dup = unsafe { State::from_ptr(state.as_mut_ptr()) };

  let e = match set_upvalue_c(state, funcindex, n).map(CStr::to_str) {
    Some(Ok(name)) => return Some(name),
    None => return None,
    Some(Err(e)) => e,
  };

  dup.pop(1);
  panic!("local name contained invalid UTF-8: {}", e);
}
