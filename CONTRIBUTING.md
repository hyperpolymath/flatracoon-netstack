<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me> -->

# Contributing Guide

Thank you for your interest in contributing to the FlatRacoon Network Stack! This guide will help you understand how to contribute effectively.

## Quick Start

```bash
# 1. Fork the repository on GitHub
# 2. Clone your fork with submodules
git clone --recursive https://github.com/YOUR-USERNAME/flatracoon-netstack.git
cd flatracoon-netstack

# 3. Install dependencies
just init

# 4. Create a branch
git checkout -b feat/your-feature-name

# 5. Make your changes

# 6. Validate
just ci

# 7. Commit with SPDX headers
git add .
git commit -m "feat: your descriptive commit message"

# 8. Push and create pull request
git push origin feat/your-feature-name
```

## Tri-Perimeter Contribution Framework

The FlatRacoon Network Stack uses a graduated trust model aligned with our security architecture. Your contribution scope depends on your role, but **everyone can contribute**!

### 🌱 Perimeter 3: Community (Open to All)

**Access**: Everyone - this is where you start!

**What You Can Contribute**:
- Documentation improvements (fix typos, clarify explanations, add diagrams)
- Bug reports and feature requests
- Example configurations
- Tutorials and guides
- Testing and QA (try things and report what breaks)
- Translations (future)

**Languages You Can Use**: Any from the approved list (see below)

**Process**:
1. Fork → make changes → validate locally → create pull request
2. CI runs automatically
3. Maintainer reviews (usually within 1 week)
4. Address feedback, get approval, merged!

### 🧠 Perimeter 2: Trusted Contributors

**Access**: Contributors with 3+ months of activity and demonstrated expertise.

**Additional Capabilities**:
- Approve PRs in your domain
- Work on module implementations
- Extend Nickel configurations
- Build testing infrastructure
- Contribute to orchestrator features

**How to Become Trusted**:
- 10+ merged contributions
- 3+ months of consistent activity
- Positive community interactions
- Self-nominate or get nominated via issue

### 🔒 Perimeter 1: Core Maintainers

**Access**: Core team only.

**Scope**:
- Orchestrator core systems
- Security-critical code
- Release management
- Architecture decisions

## Language Policy

### Approved Languages

| Language | Use Case | Notes |
|----------|----------|-------|
| **Elixir** | Orchestrator, services | Phoenix LiveView for UI |
| **Ada/SPARK** | TUI, safety-critical | Follow kith patterns |
| **ReScript** | Interface, web | Compiles to JS |
| **Nickel** | Configuration | Type-safe configs |
| **Bash** | Scripts, automation | shellcheck compliant |
| **Rust** | Performance-critical | Where Elixir insufficient |

### Banned Languages

| Banned | Replacement |
|--------|-------------|
| TypeScript | ReScript |
| JavaScript | ReScript (compiled) |
| Node.js | Deno |
| Go | Rust or Elixir |
| Python | Elixir or Rust |

**Exception**: Python only for SaltStack integration (if needed).

## Code Standards

### SPDX Headers (Required)

Every source file must have an SPDX header:

```elixir
# SPDX-License-Identifier: MPL-2.0-or-later
# SPDX-FileCopyrightText: 2025 Your Name <your.email@example.com>
```

For AsciiDoc/Markdown:
```
// SPDX-License-Identifier: MPL-2.0-or-later
// SPDX-FileCopyrightText: 2025 Your Name <your.email@example.com>
```

### Style Guides

| Language | Formatter | Linter |
|----------|-----------|--------|
| Elixir | `mix format` | `mix credo` |
| Ada | GNAT style | SPARK if safety-critical |
| ReScript | `rescript format` | Built-in |
| Nickel | `nickel format` | Built-in validation |
| Bash | `shfmt` | `shellcheck` |

### Testing Requirements

- **Unit tests**: Required for all business logic
- **Integration tests**: Required for module interactions
- **Coverage target**: 80%

Run tests:
```bash
just test                  # All tests
just orchestrator-test     # Elixir tests only
just interface-test        # ReScript tests only
```

### Security Requirements

- **No secrets**: Never commit credentials, API keys, private keys
- **Input validation**: Validate all external input
- **Dependencies**: Justify new dependencies in PR description

## Commit Message Format

We follow **Conventional Commits**:

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Test additions
- `refactor`: Code refactoring
- `build`: Build system changes
- `ci`: CI/CD changes
- `chore`: Maintenance

**Examples**:
```
feat(orchestrator): add module health aggregation

Implements health check aggregation across all modules
with configurable thresholds and alerting.

Closes #42
```

```
docs(readme): add architecture diagram

Added ASCII art diagram showing layer relationships.
```

## Creating a Pull Request

1. **Ensure CI passes locally**: `just ci`
2. **Push your branch**: `git push origin feat/your-branch`
3. **Open PR on GitHub**
4. **Fill out template**:
   - Description of changes
   - Related issues
   - Testing done
   - Screenshots (if UI)
5. **Request review**

### PR Review Process

1. **Automated checks**: CI runs tests, linting, SPDX validation
2. **Code review**: Maintainer reviews for quality and correctness
3. **Feedback**: Address requested changes
4. **Approval**: Once approved, maintainer merges
5. **Celebration** 🎉

## Types of Contributions

### 📝 Documentation

Documentation improvements are **always** welcome!

- Fix typos and grammatical errors
- Clarify confusing explanations
- Add diagrams (ASCII art or Mermaid)
- Write tutorials
- Improve examples

### 🐛 Bug Reports

Use the bug report template. Include:

- Description
- Steps to reproduce
- Expected vs actual behavior
- Environment details
- Logs/screenshots

### ✨ Feature Requests

Use the feature request template. Include:

- Problem statement
- Proposed solution
- Alternatives considered
- Willing to implement?

### 💻 Code Contributions

1. **Open issue first** (for large changes)
2. **Read existing code** to understand patterns
3. **Follow language conventions**
4. **Write tests**
5. **Update documentation**

## Development Environment

### Prerequisites

- Elixir 1.15+
- GNAT (Ada compiler)
- Deno 1.40+
- Nickel 1.4+
- Just 1.0+
- Docker/Podman (for integration tests)

### Setup

```bash
# Clone with submodules
git clone --recursive https://github.com/hyperpolymath/flatracoon-netstack.git
cd flatracoon-netstack

# Initialize (installs deps, validates tools)
just init

# Run development servers
just orchestrator-dev  # Elixir/Phoenix
just interface-run     # Deno/ReScript
just tui-run           # Ada TUI
```

### Useful Commands

```bash
just --list            # Show all available tasks
just ci                # Run all CI checks locally
just fmt               # Format all code
just lint              # Run all linters
just test              # Run all tests
just config-validate   # Validate Nickel configs
just submodules-update # Update all submodules
```

## Getting Help

- **Questions**: Open an issue with `question` label
- **Discussion**: GitHub Discussions (coming soon)
- **Chat**: Matrix (coming soon)

## Recognition

Contributors are recognized in:

- Git history (permanent)
- Release notes (significant contributions)
- CONTRIBUTORS file (generated)

## License

By contributing, you agree that your contributions will be licensed under **AGPL-3.0-or-later**.

---

**Thank you for contributing!** Together we build infrastructure that is secure, modular, and narratable.

*Last Updated: 2025-12-28*
