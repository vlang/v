## Example Workflow for Contributing

(provided by [@spytheman](https://github.com/spytheman))

(If you don't already have a GitHub account, please create one. Your GitHub
username will be referred to later as 'YOUR_GITHUB_USERNAME'. Change it
accordingly in the steps below.)

1. Fork https://github.com/ulises-jeremias/v-pendulum-simulation using GitHub's interface to your own account.
   Let's say that the forked repository is at
   `https://github.com/YOUR_GITHUB_USERNAME/v` .
2. Clone the main v-pendulum-simulation repository https://github.com/ulises-jeremias/v-pendulum-simulation to a local folder on
   your computer, say named v-pendulum-simulation/ (`git clone https://github.com/ulises-jeremias/v-pendulum-simulation v-pendulum-simulation`)
3. `cd v-pendulum-simulation`
4. `git remote add pullrequest https://github.com/YOUR_GITHUB_USERNAME/v`
   NB: the remote named `pullrequest` should point to YOUR own forked repo, not the
   main v repository! After this, your local cloned repository is prepared for
   making pullrequests, and you can just do normal git operations such as:
   `git pull` `git status` and so on.

5. When finished with a feature/bugfix/change, you can:
   `git checkout -b fix_alabala`
6. `git push pullrequest` # (NOTE: the `pullrequest` remote was setup on step 4)
7. On GitHub's web interface, go to: https://github.com/ulises-jeremias/v-pendulum-simulation/pulls

   Here the UI shows a dialog with a button to make a new pull request based on
   the new pushed branch.
   (Example dialog: https://url4e.com/gyazo/images/364edc04.png)

8. After making your pullrequest (aka, PR), you can continue to work on the
   branch `fix_alabala` ... just do again `git push pullrequest` when you have more
   commits.

9. If there are merge conflicts, or a branch lags too much behind RXV's main,
   you can do the following:

   1. `git pull --rebase origin main` # solve conflicts and do
      `git rebase --continue`
   2. `git push pullrequest -f` # this will overwrite your current remote branch
      with the updated version of your changes.

The point of doing the above steps, is to never directly push to the main RXV
repository, _only to your own fork_. Since your local `main` branch tracks the
main RXV repository's main, then `git checkout main`, as well as
`git pull --rebase origin main` will continue to work as expected
(these are actually used by `v up`) and git can always do it cleanly.

Git is very flexible, so there are other ways to accomplish the same thing.
See the [GitHub flow](https://guides.github.com/introduction/git-handbook/#github)
, for more information.

## Using Github's hub CLI tool

You can download the `hub` tool from https://hub.github.com/ . Using
`hub`, you will not need to go through the (sometimes) slow website
to make PRs. Most remote operations can be done through the `hub` CLI
command.

NB: You still need to have a GitHub account.

### Preparation:

(steps 1..3 need to be done just _once_):

1. `hub clone vlang/v-pendulum-simulation my_v-pendulum-simulation`
2. `cd my_v-pendulum-simulation`
3. `hub fork --remote-name pullrequest`

4. `git checkout -b my_cool_feature` # Step 4 is better done _once per each new
   feature/bugfix_ that you make.

### Improve RXV by making commits:

5. `git commit -am "math: add a new function copysign"`

### Testing your commits locally:

You can test locally whether your changes have not broken something by
running: `./bin/test`. See `README.md` for more details.

### Publishing your commits to GitHub:

6. `git push pullrequest`

### Making a PR with `hub`:

(so that your changes can be merged to the main RXV repository)

7. `hub pull-request`

Optionally, you can track the status of your PR CI tests with:

8. `hub ci-status --verbose`

### Fixing failing tests:

If everything is OK, after some minutes, the CI tests should pass for
all platforms. If not, visit the URLs for the failing CI jobs, see
which tests have failed and then fix them by making more changes. Just use
`git push pullrequest` to publish your changes. The CI tests will
run with your updated code. Use `hub ci-status --verbose` to monitor
their status.
