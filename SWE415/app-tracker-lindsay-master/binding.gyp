{
  "targets": [{
    "target_name": "app-tracker",
    "sources": [ ],
    "conditions": [
      ['OS=="mac"', {
        "sources": [
          "mac_active_windows.mm"
        ],
      },
      'OS=="win"', {
        "sources": [
          "win_active_windows.cpp"
        ]
      }]
    ],
    'include_dirs': [
      "<!@(node -p \"require('node-addon-api').include\")"
    ],
    'libraries': [],
    'dependencies': [
      "<!(node -p \"require('node-addon-api').gyp\")"
    ],
    'defines': [ 'NAPI_DISABLE_CPP_EXCEPTIONS' ],
    "xcode_settings": {
      "MACOSX_DEPLOYMENT_TARGET": "10.10",
      "OTHER_CPLUSPLUSFLAGS": ["-std=c++14", "-stdlib=libc++"],
      "OTHER_LDFLAGS": ["-framework CoreFoundation -framework AppKit"]
    }
  }]
}