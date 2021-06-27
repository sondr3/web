<script>
  import SEO from "$components/SEO.svelte"

  export let title
  export let description
</script>

<SEO {title} {description} />
<h1>{title}</h1>
<slot />
