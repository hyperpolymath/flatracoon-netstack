; SPDX-License-Identifier: AGPL-3.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me>
;
; FlatRacoon Network Stack - STATE.scm
; Project state and progress tracking

(define state
  '((metadata
     (version "0.1.0")
     (schema-version "1.0")
     (created "2025-12-28")
     (updated "2026-01-22")
     (project "flatracoon-netstack")
     (repo "https://github.com/hyperpolymath/flatracoon-netstack"))

    (project-context
     (name "FlatRacoon Network Stack")
     (tagline "Modular, declarative network infrastructure")
     (tech-stack
      (orchestrator "Elixir/Phoenix LiveView")
      (tui "Ada/SPARK (kith patterns)")
      (interface "Deno/ReScript")
      (configuration "Nickel")
      (task-runner "Just + mustfile")))

    (current-position
     (phase "alpha")
     (overall-completion 45)
     (components
      (flatracoon-netstack (status "documentation") (completion 40))
      (twingate-helm-deploy (status "mvp-complete") (completion 85))
      (zerotier-k8s-link (status "mvp-complete") (completion 90))
      (ipfs-overlay (status "scaffolding") (completion 10))
      (ipv6-site-enforcer (status "scaffolding") (completion 5))
      (hesiod-dns-map (status "scaffolding") (completion 5))
      (bgp-backbone-lab (status "scaffolding") (completion 5))
      (flatracoon-os (status "research") (completion 2))
      (network-dashboard (status "scaffolding") (completion 5))
      (poly-k8s-mcp (status "alpha") (completion 35))
      (poly-secret-mcp (status "alpha") (completion 25))
      (poly-observability-mcp (status "alpha") (completion 35))
      (poly-iac-mcp (status "alpha") (completion 45))
      (poly-db-mcp (status "alpha") (completion 30)))
     (working-features
      (repository-structure #t)
      (documentation #t)
      (ci-cd #t)
      (orchestrator #f)
      (tui #f)
      (interface #f)
      (core-network-layer #t)
      (core-access-layer #t)
      (mcp-integration-layer #t)))

    (route-to-mvp
     (milestone-1
      (name "Repository Foundation")
      (items
       ("Create all GitHub repos" . completed)
       ("Write README.adoc for all repos" . completed)
       ("Add SCM checkpoint files" . in-progress)
       ("Configure CI/CD workflows" . pending)
       ("Add SECURITY.md and CONTRIBUTING.md" . pending)))
     (milestone-2
      (name "Orchestrator Core")
      (items
       ("Initialize Phoenix project" . pending)
       ("Create module registry" . pending)
       ("Implement health check aggregation" . pending)
       ("Build LiveView dashboard skeleton" . pending)))
     (milestone-3
      (name "Module Integration")
      (items
       ("Link all submodules" . pending)
       ("Configure Nickel schemas" . pending)
       ("Implement deployment pipeline" . pending)
       ("Add inter-module communication" . pending)))
     (milestone-4
      (name "TUI and Interface")
      (items
       ("Port kith TUI patterns" . pending)
       ("Create ReScript bindings" . pending)
       ("Build CLI interface" . pending))))

    (blockers-and-issues
     (critical ())
     (high
      ("Need to finalize module manifest schema"))
     (medium
      ("BGP lab requires containerlab setup")
      ("FlatRacoon OS is research-only"))
     (low
      ("Documentation could use diagrams")))

    (critical-next-actions
     (immediate
      ("Complete SCM files for all repos")
      ("Add SECURITY.md with tri-perimeter model")
      ("Push initial commits to all repos"))
     (this-week
      ("Initialize Phoenix orchestrator project")
      ("Configure submodules")
      ("Add CI/CD workflows"))
     (this-month
      ("Build orchestrator module registry")
      ("Implement health check system")
      ("Create LiveView dashboard")))

    (session-history
     ((date "2025-12-28")
      (accomplishments
       ("Created 9 GitHub repositories")
       ("Wrote README.adoc for all 8 component repos")
       ("Created main flatracoon-netstack structure")
       ("Added Justfile and must.ncl")
       ("Started SCM checkpoint files")))
     ((date "2026-01-22")
      (accomplishments
       ("Completed zerotier-k8s-link to 90% (DaemonSet, scripts, Nickel configs)")
       ("Completed twingate-helm-deploy to 85% (Helm chart, NetworkPolicy)")
       ("Created reusable elixir-mcp-server framework (published)")
       ("Verified poly-* MCP component status")
       ("Updated flatracoon-netstack STATE.scm with accurate completion data")
       ("Documented MCP integration layer in ecosystem")))))))

; Helper functions
(define (get-completion-percentage)
  (cadr (assoc 'overall-completion
               (cadr (assoc 'current-position state)))))

(define (get-blockers level)
  (cadr (assoc level
               (cadr (assoc 'blockers-and-issues state)))))

(define (get-milestone n)
  (cadr (assoc (string->symbol (format "milestone-~a" n))
               (cadr (assoc 'route-to-mvp state)))))
