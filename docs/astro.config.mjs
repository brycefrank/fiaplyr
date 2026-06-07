// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	site: 'https://brycefrank.com',
	base: '/fiaplyr/',
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
				{
					label: 'Reference',
					items: [
						{ label: 'Full API Index', slug: 'reference' },
						{ label: 'eval_handler', slug: 'reference/eval_handler' },
						{ label: 'aggregate', slug: 'reference/aggregate' },
						{ label: 'estimate', slug: 'reference/estimate' },
						{ label: 'PostStratifiedEstimator', slug: 'reference/poststratifiedestimator' },
						{ label: 'filter_tree', slug: 'reference/filter_tree' },
						{ label: 'filter_cond', slug: 'reference/filter_cond' },
						{ label: 'mutate_tree', slug: 'reference/mutate_tree' },
						{ label: 'mutate_cond', slug: 'reference/mutate_cond' },
						{ label: 'set_tree_domains', slug: 'reference/set_tree_domains' },
						{ label: 'set_cond_domains', slug: 'reference/set_cond_domains' },
						{ label: 'explore_evals', slug: 'reference/explore_evals' },
					],
				},
			],
		}),
	],
});
