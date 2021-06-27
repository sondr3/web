<style lang="scss">
  .portrait {
    border-radius: 1000px;
    max-width: 100%;
    width: 100%;
  }

  .intro {
    display: flex;
    flex-direction: column;
    flex-wrap: nowrap;
    height: 100vh;
    justify-content: space-around;
    width: 100%;
  }

  .about {
    display: flex;
    flex-direction: column;
    justify-content: flex-end;

    @media screen and (min-width: $small-width) {
      flex-direction: row;
    }
  }

  .hello {
    align-items: center;
    display: flex;
    flex-direction: column;
    justify-content: center;

    @media screen and (min-width: $small-width) {
      width: 100%;
    }
  }
</style>

<script context="module">
  export const prerender = true
</script>

<script lang="ts">
  import SEO from "$components/SEO.svelte"
  import avatar from "../assets/avatar.png"
</script>

<SEO title="Home" />
<section class="intro">
  <div class="about">
    <div class="hello">
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
    </div>
    <div>
      <img class="portrait" src={avatar} alt="A stylized portrait of me." />
    </div>
  </div>
  <section>
    <p>
      I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding sideprojects and
      occasionally creating useful software.
    </p>
  </section>
</section>
