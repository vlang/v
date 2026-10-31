# Publishing module documentation

The `vlib modules CI` workflow builds V, tests vdoc, generates documentation and checks
its examples before publishing master to <https://modules.vlang.io/>. Pull requests build
and test the documentation without deploying it.

To recover from a failed build or missed deployment, run **vlib modules CI** from the
Actions tab with **Run workflow**, selecting **master**. This performs the same build,
tests and deployment as a push to master. Dispatches on other branches do not deploy.
No empty commit is needed to publish the current documentation.

Inspect the failed step and its full logs before attributing an outage to caching or
hosting credentials. A failure in **Build V** prevents vdoc and deployment from running;
fix the compiler bootstrap first, then rerun the workflow on the corrected revision.
The generation timestamp in page footers records when vdoc generated that page.

Each deployment includes `revision.txt`, containing the full source commit SHA.
The workflow checks this file at the public production URL after deployment and fails
if production still serves another revision. This checks the served artifact rather than
HTTP modification timestamps. For a manual check, compare
<https://modules.vlang.io/revision.txt> with the workflow's source revision.

A successful local build does not verify a production deployment. GitHub Actions runner
availability, repository secrets and hosting access must also be available. A pending run
has not built or published documentation yet.
