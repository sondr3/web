import React from "react";
import useAuthorMetadata from "../../hooks/useAuthorMetadata";

import dev from "../../../assets/developer.svg";

const Intro: React.FC = () => {
  const meta = useAuthorMetadata();

  return (
    <section className="intro">
      <div className="about">
        <div className="hello">
          <h1>Hello!</h1>
          <h2>I&apos;m Sondre.</h2>
        </div>
        <div>
          <img className="portrait" src={dev} alt={meta.author.intro} />
        </div>
      </div>
      <section>
        <p>
          {meta.author.intro}. You can see them on my <a href={meta.social.github}>GitHub</a>.
        </p>
      </section>
    </section>
  );
};

export default Intro;
