"use strict";

const LOCALSTORAGE_KEY = "color-scheme";
let mediaQuery = window.matchMedia("(prefers-color-scheme: dark)");
let currentTheme;

function ensureScheme(desiredScheme) {
  let osScheme = mediaQuery.matches;

  // only store the preference if it's not the same as the OS one.
  if (desiredScheme === osScheme) {
    localStorage.removeItem(LOCALSTORAGE_KEY);
  } else {
    localStorage.setItem(LOCALSTORAGE_KEY, desiredScheme);
  }

  document.documentElement.style.setProperty("color-scheme", desiredScheme);
  document.documentElement.className = desiredScheme; // for :root.dark to work

  currentTheme = desiredScheme;
}

function initDarkMode() {
  let storedScheme = localStorage.getItem(LOCALSTORAGE_KEY);
  let osScheme = mediaQuery.matches;

  // When the class of the document element is changed from a script running in the <HEAD>
  // element, no CSS transition defined on the changed properties takes place.
  ensureScheme(storedScheme === null ? osScheme : storedScheme);
  mediaQuery.addEventListener("change", osDarkModeChanged);
}

function osDarkModeChanged(query) {
  let osScheme = query.matches;
  if (currentTheme !== osScheme) {
    ensureScheme(currentTheme);
  }
}

function toggleDarkMode() {
  // The easiest is to decide based on the current color scheme.
  if (currentTheme === "dark") {
    ensureScheme("light");
  } else {
    ensureScheme("dark");
  }
}

// doing this in an onload handler would let the initial color appear for a split second.
initDarkMode();