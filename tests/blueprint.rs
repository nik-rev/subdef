// NOTE: This code assumes the 'uuid' and 'serde' crates are available and
// 'Deserialize' and 'Serialize' are imported via a prelude.
#![allow(unused, clippy::unnecessary_operation, clippy::no_effect)]

use serde::Deserialize;
use serde::Serialize;
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

fn impls_partial_ord<T: PartialOrd>() {}
fn impls_ord<T: Ord>() {}
fn impls_partial_eq<T: PartialEq>() {}
fn impls_eq<T: Eq>() {}

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

    impls_partial_eq::<DataReport>();
    impls_eq::<DataReport>();

    impls_partial_eq::<Kind>();
    impls_eq::<Kind>();
    impls_ord::<Kind>();
    impls_partial_ord::<Kind>();

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
    #[subdef(skip(debug))]
    struct TopLevel {
        nested1: [_; {
            // Skips 'serde' for this level and all children
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
            // Skip 'skip_all' and apply 'serde'
            #[subdef(apply(debug), apply_recursively(eq))]
            struct Nested2 {
                // 'serde' is applied here because of 'apply_recursively' on Nested2
                inner: [_; {
                    #[subdef(apply(debug))]
                    struct Inner2;
                }],
            }
        }],
    }

    // Logical expansion assertion:
    // - TopLevel: Debug, Serialize, Deserialize (by default, all labels are applicable)
    // - Nested1: Debug (because 'serde' is skipped recursively)
    // - Inner1: Debug (inherited skip of 'serde' from Nested1)
    // - Nested2: Serialize, Deserialize (because 'skip_all' is applied and 'serde' is applied recursively)
    // - Inner2: Serialize, Deserialize (inherited apply of 'serde' from Nested2)
}

#[subdef]
struct SystemReport {
    report_id: Uuid,
    kind: [_; {
        pub enum ReportKind {
            Initial,
            Heartbeat,
            Shutdown,
        }
    }],
    application_config: [_; {
        struct ApplicationConfig {
            version: String,
            container_runtime: String,

            flags: [_; {
                struct Flags {
                    is_admin: bool,
                    is_preview_mode: bool,
                    telemetry_enabled: bool,
                }
            }],
            components: [Vec<_>; {
                struct Component {
                    name: String,
                    version: String,
                    maintainer: Option<String>,
                    target_platform: String,
                }
            }],
        }
    }],
}

#[subdef]
struct UserProfile {
    name: String,
    address: [_; {
        struct Address {
            street: String,
            city: String,
        }
    }],
    friends: [Vec<_>; {
        struct Friend {
            name: String,
        }
    }],
    status: [_; {
        enum Status {
            Online,
            Offline,
            Idle,
        }
    }],
}

fn foo(a: ReportKind) {}

// #[derive(Serialize, Deserialize)]
// struct SystemReport {
//     report_id: Uuid,
//     kind: ReportKind,
//     application_config: ApplicationConfig,
// }

// #[derive(Serialize, Deserialize)]
// struct ApplicationConfig {
//     version: String,
//     container_runtime: String,

//     flags: Flags,
//     components: Vec<Component>,
// }

// #[derive(Serialize, Deserialize)]
// pub enum ReportKind {
//     Initial,
//     Heartbeat,
//     Shutdown,
// }

// #[derive(Serialize, Deserialize)]
// struct Flags {
//     is_admin: bool,
//     is_preview_mode: bool,
//     telemetry_enabled: bool,
// }

// #[derive(Serialize, Deserialize)]
// struct Component {
//     name: String,
//     version: String,
//     maintainer: Option<String>,
//     target_platform: String,
// }

// nestify::nest! {
//     #[derive(Deserialize, Serialize)]
//     pub struct SystemReport {
//         #[serde(skip_serializing)]
//         report_id: Uuid,

//         kind: #[derive(Deserialize, Serialize)]
//         pub enum ReportKind {
//             Initial,
//             Heartbeat,
//             Shutdown,
//         },

//         application_config: #[derive(Deserialize, Serialize)]
//         struct ApplicationConfig {
//             version: String,
//             container_runtime: String,

//             flags: #[derive(Deserialize, Serialize)] struct Flags {
//                 is_admin: bool,
//                 is_preview_mode: bool,
//                 telemetry_enabled: bool,
//             },

//             components: Vec<#[derive(Deserialize, Serialize)] struct Component {
//                 name: String,
//                 version: String,
//                 maintainer: Option<String>,
//                 target_platform: String
//             }>
//         },

//         system_info: #[derive(Deserialize, Serialize)] struct SystemInfo {
//             core_version: String,

//             #[serde(rename = "runtime")]
//             system_runtime: String,

//             os: #[derive(Deserialize, Serialize)]
//             pub enum OperatingSystem {
//                 Linux,
//                 Windows,
//                 MacOS,
//                 // Using a tuple variant for unknown OS
//                 Other(String)
//             },

//             // Drivers section now utilizes inline enums for 'r#type'
//             drivers: #[derive(Deserialize, Serialize)] struct Drivers {
//                 backup: #[derive(Deserialize, Serialize)] struct Backup {
//                     // --- NEW: Inline Enum for Backup Driver Type ---
//                     r#type: #[derive(Deserialize, Serialize)]
//                     pub enum BackupDriverType {
//                         Local,
//                         S3,
//                         GCP,
//                     }
//                 },

//                 cache: #[derive(Deserialize, Serialize)] struct Cache {
//                     // --- NEW: Inline Enum for Cache Driver Type ---
//                     r#type: #[derive(Deserialize, Serialize)]
//                     pub enum CacheDriverType {
//                         File,
//                         Redis,
//                         Memcached,
//                     }
//                 },

//                 database: #[derive(Deserialize, Serialize)] struct Database {
//                     r#type: String, // Kept as String for flexibility
//                     version: String
//                 }
//             }
//         }
//     }
// }
