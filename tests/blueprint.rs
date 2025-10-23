// NOTE: This code assumes the 'uuid' and 'serde' crates are available and
// 'Deserialize' and 'Serialize' are imported via a prelude.

use subdef::subdef;

struct Uuid;

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
