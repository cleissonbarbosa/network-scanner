# Network Scanner Release Instructions

## Automatic Releases (Recommended)

The project uses semantic versioning with conventional commits for automatic releases.

### Commit Message Format
Use conventional commits to trigger automatic version bumps:

- `feat:` - New feature (minor version bump)
- `fix:` - Bug fix (patch version bump) 
- `feat!:` or `fix!:` - Breaking change (major version bump)
- `docs:` - Documentation changes (no version bump)
- `chore:` - Maintenance tasks (no version bump)

### Examples:
```bash
git commit -m "feat: add SNMP network discovery functionality"
git commit -m "fix: resolve timeout issues in port scanning"
git commit -m "feat!: change API interface for scanner configuration"
```

When you push to `main` branch, the CI will:
1. Analyze commits since last release
2. Determine version bump (patch/minor/major)
3. Update `package.yaml` version
4. Create a git tag
5. Trigger the release workflow
6. Build binaries for Linux, macOS, Windows
7. Create GitHub release with binaries

## Manual Releases

### Option 1: Manual Tag Creation
```bash
# Create and push a version tag
git tag v1.0.0
git push origin v1.0.0
```

### Option 2: GitHub UI Release
1. Go to repository on GitHub
2. Click "Releases" → "Create a new release"
3. Choose "Create new tag" and enter version (e.g., v1.0.0)
4. This will trigger the release workflow

### Option 3: Manual Workflow Dispatch
1. Go to "Actions" tab in GitHub
2. Select "Build and Release" workflow
3. Click "Run workflow"
4. Enter version (e.g., v1.0.0)

## Release Assets

Each release includes binaries for:
- **Linux**: `network-scanner-linux-amd64.tar.gz`
- **macOS**: `network-scanner-macos-amd64.tar.gz` 
- **Windows**: `network-scanner-windows-amd64.exe.zip`

## Version Management

The version is managed in `package.yaml`:
```yaml
version: 0.1.0.0
```

- Automatic releases update this file automatically
- For manual releases, update this file before tagging
- Use semantic versioning: MAJOR.MINOR.PATCH.BUILD

## Pre-release Versions

Tag with suffixes for pre-releases:
- `v1.0.0-alpha.1` - Alpha release
- `v1.0.0-beta.1` - Beta release 
- `v1.0.0-rc.1` - Release candidate

These will be marked as "pre-release" in GitHub.

## Troubleshooting

If a release fails:
1. Check the Actions tab for error details
2. Ensure all tests pass in CI
3. Verify system dependencies are properly configured
4. Check that version format follows semver (v1.2.3)