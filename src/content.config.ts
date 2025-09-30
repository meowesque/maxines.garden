import { defineCollection, z } from 'astro:content';
import { glob } from 'astro/loaders';

const entries = defineCollection({
	loader: glob({ base: './src/content/entries', pattern: '**/*.{md,mdx}' }),
	schema: ({ image }) =>
		z.object({
			title: z.string(),
			description: z.string(),
			pubDate: z.coerce.date(),
			updatedDate: z.coerce.date().optional(),
			heroImage: image().optional(),
			editDate: z.coerce.date().optional(),
			tags: z.array(z.string()).optional(),
			group: z.string().optional(),
		}),
});

export const collections = { entries };
