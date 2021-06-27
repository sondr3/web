<script lang="ts">
  import { config } from "$lib/config"

  export let title: string
  export let description: string | undefined

  const pageTitle = `${title} | ${config.title}`
  const pageDesc = description ?? config.description
</script>

<svelte:head>
  <meta name="description" content={pageDesc} />

  <title>{pageTitle}</title>
  <meta property="og:title" content={pageTitle} />
  <meta name="twitter:title" content={pageTitle} />
  <meta itemprop="name" content={pageTitle} />
  <meta name="application-name" content={pageTitle} />
  <meta property="og:site_name" content={pageTitle} />

  <!-- Descriptions of the page -->
  <meta name="description" content={pageDesc} />
  <meta itemprop="description" content={pageDesc} />
  <meta property="og:description" content={pageDesc} />
  <meta name="twitter:description" content={pageDesc} />

  <!-- Links of the page -->
  <link rel="canonical" href={config.url} itemprop="url" />
  <meta property="url" content={config.url} />
  <meta property="og:url" content={config.url} />
  <meta name="url" content={config.url} />
  <meta name="twitter:url" content={config.url} />

  <!-- Static Meta -->
  <meta name="twitter:card" content="summary_large_image" />
  <meta name="twitter:site" content={config.socials.twitter} />
  <meta name="twitter:creator" content={config.socials.twitter} />
  <meta name="theme-color" content="#fff6f6" />
</svelte:head>
