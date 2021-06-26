<style lang="scss">
  .nav-link {
    color: var(--color-text);
    text-decoration: none;

    &:not(:last-child) {
      margin-right: $spacing;
    }

    @media screen and (max-width: $small-width) {
      margin-left: $spacing;
      padding: $spacing 0;

      &:not(:last-child) {
        margin-right: 0;
      }
    }
  }

  .title {
    color: var(--color-text);
    font-size: 1.31951rem;
    text-transform: uppercase;
  }

  header {
    line-height: 56px;
    position: relative;
  }

  nav {
    float: right;
  }
</style>

<header>
  <a class="title" href="/">Eons</a>
  <nav>
    <a class="nav-link" href="/about/">About</a>
  </nav>
</header>
