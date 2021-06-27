<style lang="scss">
  .container {
    margin: 0 auto;
    max-width: $content-width - $spacing * 2;
    padding: 0 $spacing;

    @media screen and (max-width: $large-width) {
      max-width: $content-width - $spacing;
      padding: 0 $spacing;
    }
  }
</style>

<script>
  import Footer from "$components/Footer.svelte"
  import Header from "$components/Header.svelte"

  import "$styles/index.scss"
</script>

<div class="container">
  <Header />
  <slot />
  <Footer />
</div>
