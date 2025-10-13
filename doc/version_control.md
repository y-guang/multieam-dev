for stable repo

```bash
git remote add upstream git@github.com:y-guang/multieam-dev.git
git fetch upstream main
```

each stable branch update

```bash
git fetch upstream main
git rm . -r
git checkout upstream/main -- .
git add -A
```

then commit.