<script lang="ts">
  import "../app.scss";
</script>

<header>
  <h1>EONS :: IO ()</h1>
  <nav>
    <ul>
      <!-- <li><a href="/">projects</a></li> -->
      <!-- <li><a href="/">articles</a></li> -->
      <li><a href="/">about</a></li>
    </ul>
  </nav>
</header>
<main>
  <slot />
</main>
<footer>
  <p>
    Content (C) Sondre Nilsen, licensed under <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
  </p>
</footer>

<style lang="scss">
  header {
    display: flex;
    flex-direction: column;
    padding-top: 1rem;
    grid-area: 1 / 2 / 1 / 12;

    @media (min-width: 32em) {
      grid-area: 1 / 4 / 1 / 8;
      flex-direction: row;
      justify-content: space-between;
    }
  }

  h1 {
    font-size: 1.5rem;
    align-self: center;
    padding-bottom: 1rem;
    font-family: monospace;
  }

  ul {
    display: flex;
    gap: 30px;
    flex-direction: row;
    justify-content: center;
  }

  li {
    color: #4b5563;
    font-size: 1.25rem;
  }

  main {
    padding-top: 1rem;
    grid-area: 2 / 2 / 2 / 12;

    @media (min-width: 32em) {
      grid-area: 2 / 4 / 2 / 8;
    }
  }

  footer {
    margin: 0 auto;
    padding-bottom: 1rem;
    color: #4b5563;
    grid-area: 3 / 2 / 3 / 12;

    @media (min-width: 32em) {
      grid-area: 3 / 4 / 3 / 8;
    }
  }
</style>
