# fiaplyr Documentation Maintenance

This directory contains the Astro Starlight site for fiaplyr documentation.

Use this guide when you need to update guides, regenerate reference pages, or publish docs changes.

## What Lives Here

- Starlight site config: astro.config.mjs
- Site content: src/content/docs/
- Site assets: src/assets/ and public/
- Package scripts: package.json

## Update Workflow

The docs site is generated from two sources:

1. Hand-written guide pages in src/content/docs/guides/
2. Auto-generated API/reference pages from the R package

Typical update flow:

1. Update R code, roxygen docs, or vignettes in the repository root.
2. Rebuild generated docs with repo scripts.
3. Run the docs site locally and confirm navigation/content.
4. Commit both source and generated docs changes together.

## Commands

From repository root:

- Rscript scripts/build_guides.R
	Regenerates guide markdown consumed by the docs site.

- Rscript scripts/build_reference.R
	Regenerates API/reference pages from package documentation.

From docs/:

- npm install
	Install frontend dependencies.

- npm run dev
	Start local docs preview.

- npm run build
	Build production docs output.

- npm run preview
	Preview production build locally.

## Notes for This Project

- The deployed site uses a base path configured in astro.config.mjs.
- Prefer links and asset paths that remain valid under that base path.
- Home page splash content is in src/content/docs/index.mdx.
- Sidebar structure is configured in astro.config.mjs.

## Common Edit Targets

- Add or update guides:
	src/content/docs/guides/

- Adjust landing page cards or hero:
	src/content/docs/index.mdx

- Change nav/sidebar ordering:
	astro.config.mjs

- Update styling:
	src/styles/custom.css

## Quick Validation Checklist

Before opening a PR:

1. Run both R build scripts from repository root.
2. Run npm run build from docs/.
3. Confirm no broken internal links.
4. Confirm guide pages and reference pages both changed as expected.
5. Verify landing page assets render in local preview.

## Troubleshooting

- Missing or stale reference entries:
	Re-run scripts/build_reference.R and confirm man/ files are current.

- Guide page not updating:
	Re-run scripts/build_guides.R and verify output landed under src/content/docs/.

- Asset appears in dev but not deploy:
	Check path handling against the configured base path in astro.config.mjs.
