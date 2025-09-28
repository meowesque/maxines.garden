import { getCollection } from 'astro:content';
import rss from '@astrojs/rss';
import { SITE_DESCRIPTION, SITE_TITLE } from '../constants';

export async function GET(context) {
  const entries = await getCollection('entries');
  return rss({
    title: SITE_TITLE,
    description: SITE_DESCRIPTION,
    site: context.site,
    items: entries.map((entry) => ({
      ...entry.data,
      link: `/entries/${entry.id}/`,
    })),
  });
}
