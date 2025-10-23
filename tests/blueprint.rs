// NOTE: This code assumes the 'uuid' and 'serde' crates are available and
// 'Deserialize' and 'Serialize' are imported via a prelude.
#![allow(unused, clippy::unnecessary_operation, clippy::no_effect)]

use serde::Deserialize;
use serde::Serialize;
use static_assertions::assert_impl_all;
use static_assertions::assert_not_impl_all;
use std::fmt::Debug;
use subdef::subdef;

struct Uuid;

#[test]
fn r#struct() {
    #[subdef]
    struct Profile {
        id: u32,
        info: [_; {
            struct Info {
                age: u8,
                email: String,
            }
        }],
    }

    Profile {
        id: 1,
        info: Info {
            age: 30,
            email: String::from("a@b.com"),
        },
    };

    #[subdef]
    struct Config {
        settings: [Option<_>; {
            struct Settings {
                timeout_ms: u64,
            }
        }],
    }

    Config {
        settings: Some(Settings { timeout_ms: 5000 }),
    };

    #[subdef]
    struct Level1 {
        level2: [_; {
            pub struct Level2 {
                level3: [Vec<Option<_>>; {
                    pub struct Level3 {
                        id: u8,
                    }
                }],
            }
        }],
    }

    Level1 {
        level2: Level2 {
            level3: vec![Some(Level3 { id: 1 }), None],
        },
    };
}

#[test]
fn r#enum() {
    #[subdef]
    pub enum Event {
        // Named field
        User(
            [_; {
                struct UserData {
                    name: String,
                    id: u32,
                }
            }],
        ),
        // Unnamed field
        Session(
            [u64; {
                struct SessionId(u64);
            }],
        ),
        // Nested enum
        StateChange(
            [_; {
                enum State {
                    Active,
                    Inactive,
                }
            }],
        ),
        Ping,
    }

    Event::User(UserData {
        name: String::new(),
        id: 1,
    });
    Event::Session(64);
    Event::StateChange(State::Active);
}

#[test]
fn union() {
    #[subdef]
    union Data {
        i: [std::mem::ManuallyDrop<_>; {
            struct IntData {
                value: i32,
            }
        }],
        f: [f32; { 1 }],
    }

    Data {
        i: std::mem::ManuallyDrop::new(IntData { value: 10 }),
    };
}

#[test]
fn attribute_propagation() {
    #[subdef(derive(PartialEq, Eq))]
    struct DataReport {
        #[cfg(false)]
        uuid: Uuid,
        kind: [_; {
            #[subdef(derive(PartialOrd, Ord))]
            struct Kind {
                value: String,
            }
        }],
    }

    assert_impl_all!(DataReport: PartialEq, Eq);
    assert_impl_all!(Kind: PartialEq, Eq, PartialOrd, Ord);

    // `uiud` is not present
    DataReport {
        kind: Kind {
            value: String::new(),
        },
    };
}

#[test]
fn fine_tuned_attribute_propagation() {
    #[subdef(
        debug = derive(Debug),
        eq = derive(PartialEq, Eq)
    )]
    #[subdef(skip(debug), skip(eq))]
    struct TopLevel {
        nested1: [_; {
            #[subdef(skip_recursively(eq))]
            struct Nested1 {
                inner: [_; {
                    struct Inner1 {
                        data: u32,
                    }
                }],
            }
        }],
        nested2: [_; {
            #[subdef(apply_recursively(eq))]
            struct Nested2 {
                inner: [_; {
                    #[subdef(apply(debug))]
                    struct Inner2;
                }],
            }
        }],
    }

    assert_not_impl_all!(TopLevel: Debug, Eq);

    assert_impl_all!(Nested1: Debug);
    assert_not_impl_all!(Nested1: PartialEq, Eq);

    assert_not_impl_all!(Inner1: PartialEq, Eq);
    assert_impl_all!(Inner1: Debug);

    assert_impl_all!(Nested2: Eq, PartialEq);

    assert_impl_all!(Inner2: Debug, Eq, PartialEq);
}
