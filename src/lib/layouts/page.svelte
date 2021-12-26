<script>
  export let title;
  export let description;
</script>

<svelte:head>
  <title>{title} => Eons :: IO ()</title>
  <meta name="description" content="{description}" />
</svelte:head>

<section>
  <slot />
</section>
