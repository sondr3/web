<script lang="ts">
  import "../app.scss";
</script>

<header>
  <h1>EONS</h1>
  <nav>
    <ul>
      <li><a href="/">Projects</a></li>
      <li><a href="/">Posts</a></li>
      <li><a href="/">About</a></li>
    </ul>
  </nav>
</header>
<main>
  <slot />
</main>
<footer>
  <p>Hello, world!</p>
</footer>

<style lang="scss">
  header {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    padding-top: 1rem;
    grid-area: 1 / 2 / 1 / 12;

    @media (min-width: 32em) {
      grid-area: 1 / 4 / 1 / 8;
    }
  }

  ul {
    display: flex;
    flex-direction: row;
  }

  main {
    grid-area: 2 / 2 / 2 / 12;

    @media (min-width: 32em) {
      grid-area: 2 / 4 / 2 / 8;
    }
  }

  footer {
    height: 40px;
    grid-area: 3 / 2 / 3 / 12;

    @media (min-width: 32em) {
      grid-area: 3 / 4 / 3 / 8;
    }
  }
</style>
