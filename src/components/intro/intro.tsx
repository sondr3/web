import React from "react";
import styled from "styled-components";
import Wrapper from "../common/wrapper";
import useAuthorMetadata from "../../hooks/useAuthorMetadata";

import dev from "../../../assets/developer.svg";

const Img = styled.img`
  width: 100%;
`;

export default function Intro() {
  const meta = useAuthorMetadata();

  return (
    <Wrapper>
      <Img src={dev} alt="Hello! I'm Sondre." />
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
      <p>
        {meta.author.intro}. You can see them on my{" "}
        <a href={meta.social.github}>GitHub</a>.
      </p>
    </Wrapper>
  );
}
