import { defineCollection, z } from "astro:content";

export const pageCollection = defineCollection({
  type: "content",
  schema: z.object({
    title: z.string(),
    subtitle: z.string().optional(),
    description: z.string().max(200, "Description should be <200 chars"),
    draft: z.boolean().optional(),
    publishedAt: z
      .string()
      .or(z.date())
      .transform((val) => new Date(val))
      .optional(),
    updatedAt: z
      .string()
      .or(z.date())
      .transform((val) => new Date(val))
      .optional(),
  }),
});

export const collections = {
  page: pageCollection,
};
