# Reuse installed offline dependencies, then freeze the reviewed source.
# .dockerignore excludes instance state and secrets from this context.
ARG QUALIFICATION_BASE=pai-public-staged-final:20260915
FROM ${QUALIFICATION_BASE}
USER root
RUN mkdir -p /agent/state && chown -R pai:pai /agent
COPY --chown=pai:pai . /workspace
USER pai
WORKDIR /workspace
CMD ["sh", "scripts/qualify_fleet.sh"]
