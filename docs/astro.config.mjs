// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';

// https://astro.build/config
export default defineConfig({
	site: 'https://brycefrank.com',
	base: '/fiaplyr/',
	markdown: {
		remarkPlugins: [remarkMath],
		rehypePlugins: [rehypeKatex],
	},
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
						{ label: 'Growth, Removals and Mortality', slug: 'guides/growth_removals_mortality' },
					],
				},
				{
					label: 'Reference',
					items: [
						{ label: 'Full API Index', slug: 'reference' },
            { 
              label: 'Handlers',
              items: [
                {label: 'eval_handler', slug: 'reference/eval_handler' },
                {label: 'transform', slug: 'reference/transform' },
                { label: 'augment', slug: 'reference/augment' },
                {label: 'subset', slug: 'reference/subset' },
                {label: 'partition', slug: 'reference/partition' },
                { label: 'aggregate', slug: 'reference/aggregate' },
              ]
            },
            {
              label: 'Analysis Specifications',
              items: [
                {label: 'status_analysis', slug: 'reference/status_analysis'},
                {label: 'grm_analysis', slug: 'reference/grm_analysis'}
              ]
            },
            {
              label: 'Estimators',
              items: [
                {
                  label: 'Point Estimators',
                  items: [
                    {label: 'pe_post_strat', slug: 'reference/pe_post_strat'},
                    {label: 'pe_post_strat_ratio', slug: 'reference/pe_post_strat_ratio'}
                  ]
                },
                {
                  label: 'Variance Estimators',
                  items: [
                    {label: 've_post_strat', slug: 'reference/ve_post_strat'},
                    {label: 've_post_strat_ratio', slug: 'reference/ve_post_strat_ratio'}
                  ]
                }
              ]
            }
					],
				},
			],
		}),
	],
});
