<svelte:head>
  <title>Home => Eons :: IO ()</title>
  <meta name="description" content="The online home for Sondre Nilsen" />
</svelte:head>

<main class="main">
  <section>
    <h1 class="hello">
      <span>Hello! I&apos;m</span>
      <span class="blue"> Sondre</span>
    </h1>
    <p class="body">
      I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding sideprojects and
      occasionally creating useful software.
    </p>
  </section>
</main>

<style lang="scss">
@use "../variables.scss" as v;

.main {
  max-width: v.$max-width;
  margin: 0 auto;
  padding: 4rem 0;
}

.hello {
  font-size: 3rem;
  line-height: 2.5rem;
  font-weight: 800;
}

.blue {
  color: rgba(79,70,229);
}

.body {
  margin-top: 0.5rem;
  font-size: 1.5rem;
	color: rgba(107,114,128);
}
</style>
