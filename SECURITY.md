# Security policy

pAI is a pre-release developer preview. There is no supported production release
or security-maintenance window yet.

Report vulnerabilities privately through
[GitHub private vulnerability reporting](https://github.com/jamiebeach/pAI/security/advisories/new).
Do not include credentials, private state, event databases, captured conversations,
or exploit details in a public issue. If private reporting is unavailable, open a
minimal public issue asking the maintainer to enable a private channel.

Reports should name the affected commit, describe the boundary crossed, and give
the smallest synthetic reproduction possible. pAI's local HTTP listener requires
deployment-layer TLS when exposed beyond a trusted host; authentication does not
provide transport encryption.
