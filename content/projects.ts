export interface Project {
  id: number;
  name: string;
  description: string;
  featured: boolean;
  technologies: string[];
  github: string;
}

export const projects: { [type: string]: Project[] } = {
  personal: [
    {
      id: 0,
      name: "git ignore",
      description: "A small and simple .gitignore generator",
      featured: true,
      technologies: ["Rust"],
      github: "https://github.com/sondr3/git-ignore/"
    },
    {
      id: 1,
      name: "git anger management",
      description: "Ever wanted to know just how angry your commits are?",
      featured: true,
      technologies: ["Rust", "Git"],
      github: "https://github.com/sondr3/git-anger-management/"
    },
    {
      id: 2,
      name: "MOCCA: MOCCA Operational Controller for Coffee Availability",
      description: "How much coffee is left?",
      featured: true,
      technologies: ["React", "Python", "Django", "Docker", "Arduino", "C++"],
      github: "https://github.com/inf219-mocca/"
    },
    {
      id: 3,
      name: "dotfiles",
      description: "These are my dotfiles, there are many like them but these ones are mine...",
      featured: false,
      technologies: ["Nix", "NixOS"],
      github: "https://github.com/sondr3/dotfiles/"
    },
    {
      id: 4,
      name: "web",
      description: "My personal webpage",
      featured: false,
      technologies: ["Gatsby", "React", "Styled Components"],
      github: "https://github.com/sondr3/web/"
    },
    {
      id: 5,
      name: "frontend-config",
      description: "Because I hate configuration",
      featured: false,
      technologies: ["Lerna", "CI/CD", "JavaScript"],
      github: "https://github.com/sondr3/frontend-config/"
    },
    {
      id: 6,
      name: "replieswithtime",
      description:
        "A silly little Twitter bot that replies with the current time... useful for those times when you have Twitter available but no clock.",
      featured: false,
      technologies: ["Python", "Docker"],
      github: "https://github.com/sondr3/replieswithtime/"
    }
  ],
  contributor: [
    {
      id: 1,
      name: "Nixpkgs (NixOS)",
      description: "Nix package manager repository.",
      featured: false,
      technologies: ["Nix", "NixOS", "Rust"],
      github: "https://github.com/NixOS/nixpkgs"
    },
    {
      id: 2,
      name: "Fagkveld.it",
      description: "Nettside for fagkveld",
      featured: false,
      technologies: ["React", "Styled Components", "GitHub Pages"],
      github: "https://github.com/echo-uib/Fagkveld"
    }
  ]
};

export default projects;
