<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me> -->

# Security Policy

## Tri-Perimeter Security Model

The FlatRacoon Network Stack implements a **Tri-Perimeter Security Model** that defines three concentric security boundaries with graduated trust levels.

```
┌─────────────────────────────────────────────────────────────────────┐
│                     PERIMETER 3: EXTERNAL                            │
│                     (Zero Trust - Verify Everything)                 │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │                  PERIMETER 2: NETWORK                          │  │
│  │                  (Authenticated - Trust but Verify)            │  │
│  │  ┌─────────────────────────────────────────────────────────┐  │  │
│  │  │               PERIMETER 1: CORE                          │  │  │
│  │  │               (Trusted - Verified Identity)              │  │  │
│  │  │                                                          │  │  │
│  │  │   • Orchestrator internals                               │  │  │
│  │  │   • Secrets management                                   │  │  │
│  │  │   • Core configuration                                   │  │  │
│  │  │   • Privileged operations                                │  │  │
│  │  │                                                          │  │  │
│  │  └─────────────────────────────────────────────────────────┘  │  │
│  │                                                                │  │
│  │   • ZeroTier overlay mesh                                      │  │
│  │   • Private IPFS swarm                                         │  │
│  │   • Hesiod DNS                                                 │  │
│  │   • Inter-module communication                                 │  │
│  │                                                                │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                                                                      │
│   • Twingate access layer                                            │
│   • Public-facing services (if any)                                  │
│   • External API consumers                                           │
│   • NAT64/DNS64 gateway                                              │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### Perimeter 1: Core (Trusted)

**Access**: Orchestrator internals and privileged operations only.

**Trust Level**: High - verified identity, cryptographically authenticated.

**Components**:
- Elixir orchestrator core processes
- Vault/SOPS secret retrieval
- Kubernetes admin operations
- Database connections (if any)
- Signing keys and certificates

**Controls**:
- mTLS for all connections
- Capability-based access control
- Audit logging of all operations
- Secrets never logged or displayed
- SPARK-verified critical paths (where applicable)

### Perimeter 2: Network (Authenticated)

**Access**: Services on the ZeroTier overlay mesh.

**Trust Level**: Medium - authenticated network membership.

**Components**:
- ZeroTier overlay traffic
- Private IPFS swarm
- Hesiod DNS queries
- Inter-module health checks
- Dashboard internal APIs

**Controls**:
- ZeroTier encryption (256-bit)
- Private IPFS swarm key
- Network policy enforcement
- IPv6-only (no legacy IPv4 leakage)
- Certificate validation

### Perimeter 3: External (Zero Trust)

**Access**: External connections via Twingate.

**Trust Level**: None - verify everything.

**Components**:
- Twingate connector
- NAT64/DNS64 gateway
- Public health endpoints (if exposed)
- External API consumers

**Controls**:
- Twingate zero-trust verification
- Per-request authentication
- Rate limiting
- Input validation at boundary
- No direct cluster access

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

### Where to Report

**DO NOT** create public issues for security vulnerabilities.

Report vulnerabilities via:

1. **Email**: security@hyperpolymath.dev (preferred)
2. **GitHub Security Advisories**: [Create advisory](https://github.com/hyperpolymath/flatracoon-netstack/security/advisories/new)

### What to Include

- **Description**: Clear explanation of the vulnerability
- **Impact**: What an attacker could achieve
- **Affected Components**: Which modules/layers
- **Reproduction Steps**: How to trigger the issue
- **Suggested Fix**: If you have one

### Response Timeline

| Stage | Timeline |
|-------|----------|
| Acknowledgment | 48 hours |
| Initial Assessment | 7 days |
| Fix Development | 30 days (90 for complex) |
| Disclosure | Coordinated, after fix |

### Disclosure Policy

We follow **Coordinated Disclosure**:

1. Reporter contacts us privately
2. We acknowledge and assess
3. We develop and test fix
4. We release fix
5. We publicly disclose (with credit if desired)

## Security Best Practices

### For Operators

1. **Secrets Management**
   - Use poly-secret-mcp with Vault or SOPS
   - Never store secrets in git
   - Rotate credentials regularly
   - Use short-lived tokens where possible

2. **Network Security**
   - Keep ZeroTier network private
   - Regularly audit authorized nodes
   - Use network flow rules
   - Enable IPv6-only mode

3. **Access Control**
   - Principle of least privilege
   - Regular access reviews
   - Audit log monitoring
   - Multi-factor authentication for admin access

4. **Updates**
   - Keep all components updated
   - Subscribe to security advisories
   - Test updates in staging first
   - Have rollback procedures ready

### For Contributors

1. **Code Security**
   - No hardcoded secrets (ever)
   - Validate all input at boundaries
   - Use safe APIs (no shell injection, SQL injection, etc.)
   - Follow language-specific security guidelines

2. **Dependencies**
   - Minimize dependencies
   - Pin versions explicitly
   - Run `cargo audit`, `mix audit`, etc.
   - Review dependency licenses

3. **Testing**
   - Write security-focused tests
   - Test boundary conditions
   - Include negative test cases
   - Run SAST tools

## Security Features

### Encryption

| Layer | Encryption |
|-------|------------|
| ZeroTier | 256-bit Salsa20/Poly1305 |
| IPFS Swarm | Swarm key encryption |
| Twingate | TLS 1.3 |
| Inter-service | mTLS (planned) |

### Authentication

| Component | Method |
|-----------|--------|
| Twingate | OIDC/SAML + Device Trust |
| ZeroTier | Network membership + Flow Rules |
| IPFS | Swarm key |
| Orchestrator | Capability tokens |

### Audit Logging

All security-relevant events are logged:

- Authentication attempts (success/failure)
- Authorization decisions
- Configuration changes
- Secret access
- Network policy changes

Logs are structured (JSON) and sent to poly-observability-mcp.

## Compliance

### Standards Alignment

- **OWASP**: Top 10 mitigations
- **CIS**: Kubernetes Benchmark
- **NIST**: Cybersecurity Framework
- **Zero Trust**: Architecture principles

### Certifications

None currently. This is a community project.

## Security Contacts

- **Primary**: security@hyperpolymath.dev
- **Backup**: Create GitHub Security Advisory

## Acknowledgments

We thank all security researchers who responsibly disclose vulnerabilities. Contributors are acknowledged in release notes (unless anonymity is requested).

---

*This security policy is reviewed quarterly and updated as needed.*

*Last Updated: 2025-12-28*
