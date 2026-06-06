// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'fiaplyr',
			customCss : [
				'./src/styles/custom.css'
			],
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/brycefrank/fiaplyr' }],
			sidebar: [
				{
					label: 'Guides',
					items: [
						// Each item here is one entry in the navigation menu.
						{ label: 'Getting Started', slug: 'guides/getting_started' },
						{ label: 'Status Estimates', slug: 'guides/status_estimates' },
						{ label: 'Ratio Estimates', slug: 'guides/ratio_estimates' },
					],
				},
			],
		}),
	],
});
