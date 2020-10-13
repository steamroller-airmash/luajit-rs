extern crate luajit;

use luajit::{LuaError, OwnedState, ThreadStatus};

#[test]
fn do_valid_string() {
  let mut state = unsafe { OwnedState::new() };
  state.open_libs();

  let status = state.do_string(r#"print("Hello world!")"#);
  assert!(status == Ok(ThreadStatus::Normal));
}

#[test]
fn do_invalid_string() {
  let mut state = unsafe { OwnedState::new() };
  state.open_libs();

  let status = state.do_string("aqdw98hdqw");
  assert!(status == Err(LuaError::Syntax));
}
