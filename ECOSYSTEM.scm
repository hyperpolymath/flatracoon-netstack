; SPDX-License-Identifier: AGPL-3.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me>
;
; FlatRacoon Network Stack - ECOSYSTEM.scm
; Position in the broader ecosystem and relationships

(ecosystem
 (version "1.0")
 (name "flatracoon-netstack")
 (type "infrastructure-platform")
 (purpose "Modular, declarative network stack integrating secure access,
           encrypted overlay networking, and distributed storage")

 (position-in-ecosystem
  (category "private-network-infrastructure")
  (scope "organization-internal")
  (deployment-targets
   ("kubernetes" . primary)
   ("bare-metal" . secondary)
   ("flatracoon-os" . future)))

 (related-projects

  ;; Core Component Modules (submodules)
  (twingate-helm-deploy
   (relationship module)
   (layer "access")
   (status "active"))

  (zerotier-k8s-link
   (relationship module)
   (layer "overlay")
   (status "active"))

  (ipfs-overlay
   (relationship module)
   (layer "storage")
   (status "active"))

  (ipv6-site-enforcer
   (relationship module)
   (layer "network")
   (status "active"))

  (hesiod-dns-map
   (relationship module)
   (layer "naming")
   (status "active"))

  (bgp-backbone-lab
   (relationship module)
   (layer "network")
   (status "active"))

  (flatracoon-os
   (relationship module)
   (layer "platform")
   (status "research"))

  (network-dashboard
   (relationship module)
   (layer "observability")
   (status "active"))

  ;; MCP Integrations (submodules)
  (poly-k8s-mcp
   (relationship integration)
   (purpose "Kubernetes orchestration via MCP")
   (provides ("kubectl" "helm" "kustomize")))

  (poly-secret-mcp
   (relationship integration)
   (purpose "Secrets management via MCP")
   (provides ("vault" "sops")))

  (poly-observability-mcp
   (relationship integration)
   (purpose "Observability via MCP")
   (provides ("prometheus" "grafana" "loki" "jaeger")))

  ;; Sibling Standards
  (rhodium-standard-repositories
   (relationship sibling-standard)
   (purpose "Repository and code quality standards")
   (status "compliance-target"))

  (mustfile
   (relationship sibling-standard)
   (purpose "Type-safe deployment orchestration")
   (status "integrated"))

  ;; Related Infrastructure
  (kith
   (relationship inspiration)
   (purpose "Ada TUI patterns")
   (status "pattern-source"))

  (indieweb2-bastion
   (relationship potential-integration)
   (purpose "Bastion ingress, IPv6-native DNS")
   (status "future"))

  (ipv6-only
   (relationship potential-integration)
   (purpose "IPv6 networking toolkit")
   (status "future"))

  (hybrid-automation-router
   (relationship potential-integration)
   (purpose "BGP-like config routing")
   (status "future"))

  ;; External Dependencies
  (twingate
   (relationship external-service)
   (type "zero-trust-access")
   (url "https://twingate.com"))

  (zerotier
   (relationship external-service)
   (type "overlay-networking")
   (url "https://zerotier.com"))

  (ipfs
   (relationship external-service)
   (type "distributed-storage")
   (url "https://ipfs.io"))

  (flatcar-linux
   (relationship external-dependency)
   (type "base-os")
   (url "https://flatcar-linux.org"))

  (minix
   (relationship inspiration)
   (type "microkernel-os")
   (url "https://minix3.org")))

 (what-this-is
  ("A complete private network infrastructure stack")
  ("An orchestration layer for network components")
  ("A reference architecture for secure overlay networks")
  ("A platform for distributed, encrypted storage")
  ("A lab environment for BGP and routing experiments"))

 (what-this-is-not
  ("A public cloud provider")
  ("A replacement for enterprise networking vendors")
  ("A production-ready OS (flatracoon-os is research)")
  ("A one-size-fits-all solution")))

; Media type for this format
(define ecosystem-media-type "application/vnd.ecosystem+scm")
