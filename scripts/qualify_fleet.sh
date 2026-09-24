#!/bin/sh
# Offline qualification; invoke inside a disposable Linux container.
set -u
cd /workspace
export PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp
export PAI_TEST_LOG_DIR=/tmp/fleet-suite-logs
export PAI_TEST_STATE=/tmp/fleet-suite-state
status=0
sbcl --dynamic-space-size 2048 --non-interactive \
  --load "$PAI_QUICKLISP_SETUP" --eval '(require :asdf)' \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(asdf:load-system "pai")' > /tmp/fleet-offline-load.log 2>&1
load_status=$?
printf 'Offline load exit: %s\n' "$load_status"
if [ "$load_status" -ne 0 ]; then
  tail -80 /tmp/fleet-offline-load.log
  exit "$load_status"
fi
sbcl --script tests/wrap-chain-completeness-tests.lisp src/ > /tmp/fleet-wrap.log 2>&1
wrap_status=$?
tail -5 /tmp/fleet-wrap.log
if [ "$wrap_status" -ne 0 ]; then status=1; fi
sh tests/run-isolated.sh sbcl /workspace "${1:-*-tests.lisp}"
suite_status=$?
if [ "$suite_status" -ne 0 ]; then status=1; fi
# Docker tmpfs /tmp may be noexec; host-launcher fixtures execute fake SBCL.
TMPDIR=$(mktemp -d /home/pai/.cache/fleet-host-tests.XXXXXX)
export TMPDIR
for pattern in test_publication.py test_isolated_runner.py test_qualification_contract.py; do
  python3 -m unittest discover -s tests -p "$pattern" -v > "/tmp/$pattern.log" 2>&1
  python_status=$?
  printf 'Python %s exit: %s\n' "$pattern" "$python_status"
  tail -5 "/tmp/$pattern.log"
  if [ "$python_status" -ne 0 ]; then status=1; fi
done
python3 scripts/qualification_contract.py
if [ "$?" -ne 0 ]; then status=1; fi
exit "$status"
