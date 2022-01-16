import { resumeData as data } from "../content/resume.mjs";
import { html } from "../dist/templates/html.js";

export default (_content) => html`
  <h1>Resume</h1>
  <p>${data.about}</p>

  <h2>Education</h2>
  ${data.education.map((edu) => html`
    <ul>
      ${edu.degrees.map((deg) => html`
        <li>
          <h4>${deg.university}
            <span class="degree__time">${deg.start} - ${deg.end}</span>
          </h4>
          <p>${deg.degree}, ${deg.title}</p>
        </li>
      `)}
    </ul>
  `)}

  <h2>Experience</h2>
  ${data.experience.map((exp) => html`
    <h3>${exp.position}, ${exp.company}</h3>
    <p>${exp.start} - ${exp.end}</p>
    <ul>${exp.about.map((a) => html`
      <li>${a}</li>`)}
    </ul>
    <ul>${exp.technologies.map((tech) => html`
      <li>${tech}</li>`)}
    </ul>
  `)}
`
