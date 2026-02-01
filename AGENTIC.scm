; SPDX-License-Identifier: MPL-2.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me>
;
; FlatRacoon Network Stack - AGENTIC.scm
; AI agent guidance and interaction patterns

(define agentic
  '((metadata
     (version "1.0")
     (project "flatracoon-netstack")
     (updated "2025-12-28"))

    (agent-context
     (project-type "infrastructure-platform")
     (complexity "high")
     (languages ("Elixir" "Ada" "ReScript" "Nickel" "Bash"))
     (key-concepts
      ("Zero-trust networking")
      ("Overlay mesh networks")
      ("Distributed storage")
      ("IPv6-only infrastructure")
      ("Microkernel architecture")
      ("Declarative configuration")))

    (interaction-patterns

     (code-generation
      (preferred-languages
       (orchestrator "Elixir")
       (tui "Ada")
       (interface "ReScript")
       (configuration "Nickel")
       (scripting "Bash"))
      (banned-languages ("TypeScript" "Node.js" "Go" "Python"))
      (style-guides
       ("Elixir: mix format + credo")
       ("Ada: GNAT style, SPARK annotations where safety-critical")
       ("ReScript: standard formatter")
       ("Nickel: standard formatter")
       ("Bash: shellcheck compliant")))

     (documentation
      (format "AsciiDoc")
      (style "Technical, precise, narratable")
      (required-elements
       ("Purpose statement")
       ("Architecture diagram (ASCII)")
       ("Inputs/Outputs table")
       ("Integration points")
       ("Machine-readable manifest")))

     (configuration
      (format "Nickel")
      (validation "Always validate before deployment")
      (secrets "Never hardcode, use poly-secret-mcp")
      (patterns
       ("Use type contracts")
       ("Compose configurations")
       ("Document all fields")))

     (deployment
      (approach "Declarative, via Justfile")
      (verification "Health checks after each module")
      (rollback "Always have rollback plan")
      (order "Follow deployment_order in must.ncl")))

    (knowledge-requirements

     (networking
      (protocols ("IPv6" "BGP" "DNS" "mDNS" "IPFS" "ZeroTier"))
      (concepts ("NAT traversal" "Overlay networks" "Mesh topology" "Route convergence"))
      (tools ("kubectl" "zerotier-cli" "ipfs" "dig" "ip")))

     (kubernetes
      (resources ("Deployment" "StatefulSet" "DaemonSet" "Service" "ConfigMap" "Secret" "NetworkPolicy"))
      (tools ("kubectl" "helm" "kustomize"))
      (patterns ("Sidecar" "Init containers" "Operators")))

     (security
      (concepts ("Zero-trust" "mTLS" "Capability-based security" "Principle of least privilege"))
      (tools ("Vault" "SOPS" "Network policies"))
      (considerations ("Never expose secrets" "Rotate credentials" "Audit access"))))

    (task-patterns

     (add-new-module
      (steps
       (1 "Create GitHub repo with README.adoc")
       (2 "Define machine-readable manifest")
       (3 "Create Nickel configuration schema")
       (4 "Add Justfile with standard targets")
       (5 "Add as submodule to flatracoon-netstack")
       (6 "Register in orchestrator")
       (7 "Add health check endpoint")
       (8 "Update documentation")))

     (debug-connectivity
      (approach
       (1 "Verify layer by layer (L1 → L7)")
       (2 "Check health endpoints")
       (3 "Review logs for errors")
       (4 "Verify configuration")
       (5 "Test with known-good configuration")))

     (implement-feature
      (approach
       (1 "Understand existing patterns")
       (2 "Check META.scm for architecture decisions")
       (3 "Follow language-specific conventions")
       (4 "Add tests")
       (5 "Update STATE.scm")
       (6 "Document changes"))))

    (guardrails

     (never-do
      ("Hardcode secrets or credentials")
      ("Use banned languages (TypeScript, Go, Python)")
      ("Deploy without health verification")
      ("Modify production without rollback plan")
      ("Expose internal services publicly")
      ("Skip SPDX headers"))

     (always-do
      ("Validate Nickel configs before use")
      ("Check STATE.scm for current context")
      ("Follow deployment order")
      ("Add health checks to new modules")
      ("Document architectural decisions in META.scm")
      ("Update ECOSYSTEM.scm when adding integrations")))

    (prompt-templates

     (new-session
      "Starting work on FlatRacoon Network Stack.
       Please read STATE.scm, META.scm, and ECOSYSTEM.scm first.
       Current phase: {phase}, completion: {percentage}%
       Key focus: {critical-next-actions}")

     (deployment-task
      "Deploying {module} to {environment}.
       Prerequisites: {prerequisites}
       Follow PLAYBOOK.scm procedure: {procedure-name}")

     (troubleshooting
      "Issue: {symptom}
       Affected module: {module}
       Check PLAYBOOK.scm troubleshooting section: {section-name}"))))

; Helper to get agent context for a specific task type
(define (get-task-pattern name)
  (assoc name (cadr (assoc 'task-patterns agentic))))

; Helper to get guardrails
(define (get-guardrails type)
  (cadr (assoc type (cadr (assoc 'guardrails agentic)))))
