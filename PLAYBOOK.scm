; SPDX-License-Identifier: AGPL-3.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath <hyperpolymath@proton.me>
;
; FlatRacoon Network Stack - PLAYBOOK.scm
; Operational runbooks and procedures

(define playbook
  '((metadata
     (version "1.0")
     (project "flatracoon-netstack")
     (updated "2025-12-28"))

    (deployment-procedures

     (full-stack-deployment
      (name "Deploy Complete FlatRacoon Stack")
      (prerequisites
       ("Kubernetes cluster available")
       ("kubectl configured")
       ("Secrets configured in Vault")
       ("Twingate network created")
       ("ZeroTier network created"))
      (steps
       (1 "Validate configuration" "just config-validate")
       (2 "Deploy access layer" "just deploy-access")
       (3 "Verify Twingate connector" "just health-access")
       (4 "Deploy overlay network" "just deploy-overlay")
       (5 "Authorize ZeroTier nodes" "just authorize-nodes")
       (6 "Verify mesh connectivity" "just health-overlay")
       (7 "Deploy IPFS cluster" "just deploy-storage")
       (8 "Verify IPFS swarm" "just health-storage")
       (9 "Deploy IPv6 enforcer" "just deploy-network")
       (10 "Deploy Hesiod DNS" "just deploy-naming")
       (11 "Deploy dashboard" "just deploy-observability")
       (12 "Full health check" "just health"))
      (rollback "just rollback-all")
      (verification "just health && just dashboard"))

     (module-update
      (name "Update Single Module")
      (steps
       (1 "Check current version" "just module-version MODULE")
       (2 "Pull latest submodule" "cd modules/MODULE && git pull")
       (3 "Validate new configuration" "just config-validate")
       (4 "Deploy update" "cd modules/MODULE && just deploy")
       (5 "Verify health" "just health-MODULE")
       (6 "Update orchestrator registry" "just registry-refresh"))
      (rollback "cd modules/MODULE && git checkout PREVIOUS_SHA && just deploy")))

    (troubleshooting

     (twingate-connection-failure
      (symptoms
       ("Connector pod not ready")
       ("Authentication errors in logs")
       ("Unable to reach protected resources"))
      (diagnosis
       (1 "Check pod status" "kubectl get pods -l app=twingate")
       (2 "Check connector logs" "kubectl logs -l app=twingate")
       (3 "Verify secrets" "kubectl get secret twingate-credentials")
       (4 "Check network policy" "kubectl get networkpolicy"))
      (resolution
       ("Rotate Twingate tokens if expired")
       ("Verify network ID matches")
       ("Check firewall rules for outbound 443")))

     (zerotier-mesh-incomplete
      (symptoms
       ("Nodes not seeing each other")
       ("High latency between nodes")
       ("NAT traversal failing"))
      (diagnosis
       (1 "Check ZeroTier status" "zerotier-cli info")
       (2 "List peers" "zerotier-cli peers")
       (3 "Check network membership" "zerotier-cli listnetworks")
       (4 "Verify node authorization" "ZeroTier Central UI"))
      (resolution
       ("Authorize nodes in ZeroTier Central")
       ("Check firewall for UDP 9993")
       ("Enable port mapping if behind NAT")))

     (ipfs-swarm-disconnected
      (symptoms
       ("Zero peers connected")
       ("Content not resolving")
       ("Bootstrap failures"))
      (diagnosis
       (1 "Check swarm peers" "ipfs swarm peers")
       (2 "Verify swarm key" "cat ~/.ipfs/swarm.key")
       (3 "Check bootstrap config" "ipfs config Bootstrap")
       (4 "Verify ZeroTier interface" "ip addr show zt*"))
      (resolution
       ("Ensure swarm key matches across nodes")
       ("Verify IPFS binds to ZeroTier interface")
       ("Check bootstrap addresses are reachable")))

     (ipv6-traffic-blocked
      (symptoms
       ("IPv4 connections succeeding")
       ("IPv6 connections timing out")
       ("NAT64 not working"))
      (diagnosis
       (1 "Check IPv6 connectivity" "ping6 ipv6.google.com")
       (2 "Verify NAT64 prefix" "dig AAAA ipv4only.arpa")
       (3 "Check DNS64" "dig AAAA google.com @dns64-server")
       (4 "Verify firewall rules" "ip6tables -L"))
      (resolution
       ("Verify NAT64/DNS64 deployment")
       ("Check IPv6 routing tables")
       ("Ensure firewall allows IPv6"))))

    (maintenance-procedures

     (certificate-rotation
      (schedule "quarterly")
      (steps
       (1 "Generate new certificates")
       (2 "Update Vault secrets")
       (3 "Rolling restart of services")
       (4 "Verify TLS connections")))

     (backup-ipfs-pins
      (schedule "daily")
      (steps
       (1 "Export pin list" "ipfs pin ls --type=recursive > pins.txt")
       (2 "Backup to external storage")
       (3 "Verify backup integrity")))

     (zerotier-network-audit
      (schedule "monthly")
      (steps
       (1 "Export member list from Central")
       (2 "Compare to expected nodes")
       (3 "Remove stale authorizations")
       (4 "Review flow rules"))))

    (emergency-procedures

     (security-incident
      (severity "critical")
      (steps
       (1 "Isolate affected nodes" "kubectl cordon NODE")
       (2 "Revoke Twingate access" "Twingate Admin Console")
       (3 "Deauthorize ZeroTier nodes" "ZeroTier Central")
       (4 "Rotate all credentials")
       (5 "Audit access logs")
       (6 "Document incident")))

     (data-loss-recovery
      (severity "high")
      (steps
       (1 "Identify lost content CIDs")
       (2 "Check other IPFS nodes for pins")
       (3 "Restore from backup if available")
       (4 "Re-pin recovered content")
       (5 "Update backup procedures"))))))

; Helper to get a specific procedure
(define (get-procedure category name)
  (assoc name (cadr (assoc category playbook))))
