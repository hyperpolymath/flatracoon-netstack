; SPDX-License-Identifier: MPL-2.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me>
;
; FlatRacoon Network Stack - META.scm
; Architecture decisions and development practices

(define meta
  '((architecture-decisions

     (adr-001
      (title "Modular Submodule Architecture")
      (status accepted)
      (date "2025-12-28")
      (context "Need to compose multiple independent networking components
                into a cohesive stack while maintaining independent deployability")
      (decision "Use git submodules for each component, with the main repo
                 as orchestration layer only")
      (consequences
       ("+ Components can evolve independently")
       ("+ Clear ownership boundaries")
       ("+ Can be deployed standalone or integrated")
       ("- Submodule management complexity")
       ("- Need to keep manifests in sync")))

     (adr-002
      (title "Elixir/Phoenix for Orchestrator")
      (status accepted)
      (date "2025-12-28")
      (context "Need fault-tolerant, real-time orchestration with live dashboard")
      (decision "Use Elixir with Phoenix LiveView for orchestrator and dashboard")
      (consequences
       ("+ Battle-tested fault tolerance (BEAM)")
       ("+ Real-time updates via LiveView")
       ("+ Excellent for supervision trees")
       ("- Team needs Elixir knowledge")
       ("- Larger runtime than Go/Rust")))

     (adr-003
      (title "Ada/SPARK for TUI")
      (status accepted)
      (date "2025-12-28")
      (context "Need reliable, type-safe TUI following kith patterns")
      (decision "Use Ada/SPARK with patterns from hyperpolymath/kith")
      (consequences
       ("+ Maximum type safety")
       ("+ Proven patterns from kith")
       ("+ SPARK subset for critical paths")
       ("- Limited Ada TUI libraries")
       ("- Steeper learning curve")))

     (adr-004
      (title "Nickel for Configuration")
      (status accepted)
      (date "2025-12-28")
      (context "Need type-safe, composable configuration language")
      (decision "Use Nickel with mustfile integration for all configuration")
      (consequences
       ("+ Type-checked configurations")
       ("+ Composable and modular")
       ("+ Integrates with Just via mustfile")
       ("- Nickel still maturing")
       ("- Need to write schemas")))

     (adr-005
      (title "IPv6-First Networking")
      (status accepted)
      (date "2025-12-28")
      (context "Modern network stack should be IPv6-native")
      (decision "Enforce IPv6-only with NAT64/DNS64 for legacy compatibility")
      (consequences
       ("+ Future-proof addressing")
       ("+ Simpler routing (no NAT)")
       ("+ Better security posture")
       ("- Some legacy clients need NAT64")
       ("- Additional complexity for transition")))

     (adr-006
      (title "Hesiod for Service Discovery")
      (status accepted)
      (date "2025-12-28")
      (context "Need service discovery without heavy dependencies")
      (decision "Use Hesiod (HS-class DNS) for lightweight service discovery")
      (consequences
       ("+ DNS-native, no new protocols")
       ("+ Simple to implement")
       ("+ Works with existing tooling")
       ("- Less feature-rich than Consul/etcd")
       ("- Requires HS-class DNS support"))))

    (development-practices
     (code-style
      (languages
       ("Elixir" . "mix format + credo")
       ("Ada" . "GNAT style + SPARK subset where applicable")
       ("ReScript" . "rescript format")
       ("Nickel" . "nickel format")
       ("Bash" . "shellcheck + shfmt"))
      (line-length 100)
      (indentation "2 spaces for most, 3 for Ada"))

     (security
      (secrets "Never in repo, use poly-secret-mcp")
      (dependencies "Pin versions, audit regularly")
      (network "Zero-trust, mTLS where possible")
      (auth "Capability-based, principle of least privilege"))

     (testing
      (unit "Required for all business logic")
      (integration "Required for module interactions")
      (e2e "Required for deployment pipelines")
      (coverage-target 80))

     (versioning
      (scheme "SemVer")
      (changelog "CHANGELOG.md in each repo")
      (tags "v{major}.{minor}.{patch}"))

     (documentation
      (format "AsciiDoc for primary, Markdown for README")
      (api "ExDoc for Elixir, AdaDoc for Ada")
      (diagrams "Mermaid or ASCII art"))

     (branching
      (model "trunk-based")
      (main "main")
      (features "feat/{description}")
      (fixes "fix/{description}")))

    (design-rationale
     (why-microkernel-os
      "Traditional monolithic kernels have large attack surfaces and
       single points of failure. A microkernel architecture with
       user-space servers provides better fault isolation and
       self-healing capabilities, essential for critical network
       infrastructure.")

     (why-zerotier-over-wireguard
      "ZeroTier provides automatic NAT traversal, peer discovery,
       and mesh networking out of the box. WireGuard is excellent
       but requires more manual configuration for dynamic meshes.
       ZeroTier's SDN controller model fits our orchestration approach.")

     (why-ipfs-private-swarm
      "Public IPFS exposes content to the world. A private swarm
       with custom bootstrap nodes and swarm key ensures content
       stays within our network boundary while retaining the
       benefits of content-addressing and deduplication.")

     (why-hesiod-over-consul
      "Hesiod is DNS-native, requiring no additional protocols or
       services. For our use case of service discovery within a
       controlled network, Hesiod's simplicity wins over Consul's
       features we don't need."))))

; Media type for this format
(define meta-media-type "application/meta+scheme")
