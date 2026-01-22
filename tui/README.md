# FlatRacoon TUI

Terminal User Interface for the FlatRacoon Network Stack, written in Ada/SPARK.

## Features

- **Interactive shell** - Type commands interactively or use from CLI
- **Module management** - View status, deploy, restart, stop modules
- **Health monitoring** - Aggregate health checks across all modules
- **Deployment orchestration** - Shows topological deployment order
- **Colored output** - ANSI color support for better readability
- **Type-safe** - Ada/SPARK provides compile-time safety guarantees

## Commands

```
help, h              - Show help message
status, st [module]  - Show module status (all or specific)
health, hc           - Show health check summary
deploy, d [module]   - Deploy module(s)
order, o             - Show deployment order
logs, l <module>     - Show logs for module
restart, r <module>  - Restart module
stop, s <module>     - Stop module
exit, quit           - Exit TUI
```

## Building

Requires GNAT Ada compiler (Ada 2022 support):

```bash
# Build the project
gprbuild -P flatracoon_tui.gpr

# Run the TUI (interactive mode)
./bin/flatracoon_tui

# Run with command-line arguments
./bin/flatracoon_tui status
./bin/flatracoon_tui deploy zerotier-k8s-link
```

## Architecture

- **flatracoon.ads** - Root package with version info
- **flatracoon-display** - Terminal display utilities (colors, tables, banners)
- **flatracoon-commands** - Command parsing and execution
- **flatracoon-api_client** - HTTP client for orchestrator API
- **flatracoon_tui.adb** - Main entry point

## Orchestrator Integration

The TUI connects to the Phoenix LiveView orchestrator at `http://localhost:4000` by default.

API endpoints:
- `GET /api/modules` - List all modules
- `GET /api/modules/:name` - Get specific module
- `GET /api/health` - Health summary
- `GET /api/deployment_order` - Topological order
- `POST /api/deploy` - Deploy all modules
- `POST /api/deploy/:name` - Deploy specific module
- `POST /api/restart/:name` - Restart module
- `POST /api/stop/:name` - Stop module
- `GET /api/logs/:name` - Get module logs

## MVP Status

✅ Project structure
✅ Display utilities (colors, banners, tables)
✅ Command parsing and execution
✅ API client stubs (mock data)
✅ Interactive shell
⏳ HTTP client implementation (TODO)
⏳ JSON parsing (TODO)

## License

AGPL-3.0-or-later
