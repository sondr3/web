import { EmojiHappyIcon, EmojiSadIcon, InboxInIcon } from "@heroicons/react/outline"
import { FormEvent, ReactNode, useState } from "react"

import { apiEndpoint, config } from "@/lib/config"

interface ContactForm {
  name: string
  from: string
  message: string
  whoami: string
  state: "submitted" | "error" | "missing" | "pending" | "clean"
}

const defaultForm: ContactForm = {
  name: "",
  from: "",
  message: "",
  whoami: "",
  state: "clean",
}

interface NotificationProps {
  icon: ReactNode
  title: string
  text: string
}

const NotificationBox = ({ icon, title, text }: NotificationProps): JSX.Element => (
  <div className="flex justify-center text-lg text-gray-500 dark:text-gray-400 sm:text-xl">
    <div className="mr-4 flex-shrink-0 self-center">{icon}</div>
    <div className="max-w-sm">
      <h4 className="text-lg font-bold dark:text-gray-400">{title}</h4>
      <p className="mt-1">{text}</p>
    </div>
  </div>
)

export const Contact = (): JSX.Element => {
  const [state, setState] = useState<ContactForm>(defaultForm)

  const handleSubmit = async (event: FormEvent) => {
    event.preventDefault()
    if (!state.message.trim() || !state.name.trim() || !state.from.trim() || !state.whoami.trim()) {
      setState((s) => ({ ...s, state: "missing" }))
      return
    } else {
      setState((s) => ({ ...s, state: "pending" }))
      const res = await fetch(`${apiEndpoint}/${config.endpoints.contact}`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(state, undefined, 0),
      })

      if (res.status !== 200) {
        setState((s) => ({ ...s, state: "error" }))
      } else {
        setState((s) => ({ ...s, state: "submitted" }))
      }
    }
  }

  return (
    <section className="pt-16">
      <div>
        <h2 className="text-3xl font-extrabold tracking-tight text-gray-900 dark:text-gray-400 sm:text-4xl">
          Contact me
        </h2>
        <p className="mt-4 text-lg leading-6 text-gray-500 dark:text-gray-400">
          Either send me an email to first-name at this domain, or use the form below.
        </p>
      </div>
      <div className="mt-8">
        {state.state === "missing" && (
          <p className="text-red-400 pb-2 font-bold">You are missing some required fields</p>
        )}
        {state.state === "pending" && (
          <NotificationBox
            icon={<InboxInIcon className="h-16 w-16" />}
            title="Sending..."
            text="Just waiting for the mail to be delivered"
          />
        )}
        {state.state === "error" && (
          <NotificationBox
            icon={<EmojiSadIcon className="h-16 w-16" />}
            title="Oh no"
            text="Something went wrong. Maybe you wrote my name wrong? Try again or send me an email directly."
          />
        )}
        {state.state === "submitted" && (
          <NotificationBox
            icon={<EmojiHappyIcon className="h-16 w-16" />}
            title="Thanks!"
            text="I'll be in touch soon."
          />
        )}
        {!(state.state === "submitted" || state.state === "pending") && (
          <form onSubmit={handleSubmit} className="grid grid-cols-1 gap-y-6 sm:grid-cols-2 sm:gap-x-8">
            <div className="sm:col-span-2">
              <label htmlFor="first_name" className="block text-sm font-medium text-gray-700 dark:text-gray-400">
                Name
              </label>
              <div className="mt-1">
                <input
                  type="text"
                  name="first_name"
                  id="first_name"
                  autoComplete="name"
                  className="py-3 px-4 block w-full shadow-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:bg-gray-200 rounded-md"
                  onChange={(e) => setState((s) => ({ ...s, name: e.target.value }))}
                />
              </div>
            </div>
            <div className="sm:col-span-2">
              <label htmlFor="email" className="block text-sm font-medium text-gray-700 dark:text-gray-400">
                Email
              </label>
              <div className="mt-1">
                <input
                  id="email"
                  name="email"
                  type="email"
                  autoComplete="email"
                  className="py-3 px-4 block w-full shadow-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:bg-gray-200 rounded-md"
                  onChange={(e) => setState((s) => ({ ...s, from: e.target.value }))}
                />
              </div>
            </div>
            <div className="sm:col-span-2">
              <label htmlFor="message" className="block text-sm font-medium text-gray-700 dark:text-gray-400">
                Message
              </label>
              <div className="mt-1">
                <textarea
                  id="message"
                  name="message"
                  rows={4}
                  className="py-3 px-4 block w-full shadow-sm focus:ring-indigo-500 focus:border-indigo-500 border border-gray-300 dark:bg-gray-200 rounded-md"
                  onChange={(e) => setState((s) => ({ ...s, message: e.target.value }))}
                  defaultValue={""}
                />
              </div>
            </div>
            <div className="sm:col-span-2">
              <label htmlFor="whoami" className="block text-sm font-medium text-gray-700 dark:text-gray-400">
                What is my first name? <span className="text-gray-400 dark:text-gray-500">(for spam protection)</span>
              </label>
              <div className="mt-1">
                <input
                  type="text"
                  name="whoami"
                  id="whoami"
                  className="py-3 px-4 block w-full shadow-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:bg-gray-200 rounded-md"
                  onChange={(e) => setState((s) => ({ ...s, whoami: e.target.value }))}
                />
              </div>
            </div>
            <div className="sm:col-span-2">
              <button
                type="submit"
                className="w-full inline-flex items-center justify-center px-6 py-3 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
              >
                Let&apos;s talk
              </button>
            </div>
          </form>
        )}
      </div>
    </section>
  )
}
