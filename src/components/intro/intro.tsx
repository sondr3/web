import React from "react";
import styled from "styled-components";
import useAuthorMetadata from "../../hooks/useAuthorMetadata";
import c from "../../styles/constants";

import dev from "../../../assets/developer.svg";

const Portrait = styled.img`
  max-width: 100%;
  width: 100%;
`;

const Wrapper = styled.section`
  display: flex;
  flex-direction: column;
  flex-wrap: nowrap;
  height: 100vh;
  justify-content: space-around;
  width: 100%;
`;

const About = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: end;

  @media screen and (min-width: ${c.size.smallWidth}px) {
    flex-direction: row;
  }
`;

const Hello = styled.div`
  align-items: center;
  display: flex;
  flex-direction: column;
  justify-content: end;

  @media screen and (min-width: ${c.size.smallWidth}px) {
    width: 100%;
  }
`;

const Intro: React.FC = () => {
  const meta = useAuthorMetadata();

  return (
    <Wrapper>
      <About>
        <Hello>
          <h1>Hello!</h1>
          <h2>I&apos;m Sondre.</h2>
        </Hello>
        <div>
          <Portrait src={dev} alt={meta.author.intro} />
        </div>
      </About>
      <section>
        <p>
          {meta.author.intro}. You can see them on my <a href={meta.social.github}>GitHub</a>.
        </p>
      </section>
    </Wrapper>
  );
};

export default Intro;
