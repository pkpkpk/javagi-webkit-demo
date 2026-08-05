(ns pkpkpk.webkit
  (:require
    [malli.core :as m])
  (:import
    (org.gnome.gdk RGBA)
    (org.gnome.gio Application$ActivateCallback ApplicationFlags)
    (org.gnome.glib GLib SourceFunc)
    (org.gnome.gtk Application ApplicationWindow CssProvider Gtk StyleContext Window Window$CloseRequestCallback)
    (org.webkitgtk UserContentInjectedFrames UserContentManager$ScriptMessageReceivedCallback UserScript UserScriptInjectionTime UserStyleLevel UserStyleSheet WebView)
    (org.webkitgtk.jsc Value)))

(defonce the-app (atom nil))
(defonce app-thread (atom nil))
(defonce windows (atom {}))

;; Helper to run code on the GTK main thread safely
(defmacro on-ui [& body]
  `(GLib/idleAdd 200
                 (fn []
                   (try
                     ~@body
                     (catch Exception e# (println "UI Error:" e#)))
                   false)))

(defn- start-background-loop! []
  (let [promise-app (promise)
        thread      (Thread. ^Runnable
                             (bound-fn []
                               (try
                                 (println "Initializing Singleton GTK Application...")
                                 (let [app (Application. "org.example.clojure.suite"
                                                         ^ApplicationFlags/1 (into-array ApplicationFlags [ApplicationFlags/DEFAULT_FLAGS]))]
                                   (.hold app)              ;; Keep alive for REPL
                                   ;; Attach dummy activate handler to silence GLib warning
                                   (.onActivate app (reify Application$ActivateCallback (run [_])))
                                   (deliver promise-app app)
                                   (reset! the-app app)
                                   (println "Starting GTK Event Loop...")
                                   (.run app (into-array String []))
                                   (println "GTK Event Loop Exited."))
                                 (catch Exception e
                                   (println "Fatal GTK Error:" e)
                                   (deliver promise-app nil)))))]
    (.setName thread "GTK-Main-Thread")
    (.start thread)
    (reset! app-thread thread)
    @promise-app))

(defn ^Application ensure-app-running! []
  (if @the-app
    @the-app
    (start-background-loop!)))

#!---------------------------------------------------------------------------------------------------------------------
#! edn bindings

(def transparent-css
     (str
       "window.transparent, window.transparent.background, window.transparent .background {"
       "  background-color: rgba(0,0,0,0);"
       "}\n"))

(defn apply-transparent-style! [^Window window]
  (.addCssClass window "transparent")
  (let [provider (CssProvider.)
        context  (.getStyleContext window)]
    (.loadFromString provider transparent-css)
    (.addProvider context provider Gtk/STYLE_PROVIDER_PRIORITY_APPLICATION)))

(defn ^ApplicationWindow ApplicationWindow:from-edn
  [instance {:keys [title width height decorated transparent opacity] :as arg}]
  (assert (instance? ApplicationWindow instance))
  (some->> title (.setTitle instance))
  (some->> decorated (.setDecorated instance))
  (some->> opacity (.setOpacity instance))
  (.setDefaultSize instance (or width 800) (or height 600))
  (when transparent (apply-transparent-style! instance))
  instance)

(defn ^RGBA RGBA:from-edn
  "supports map, vector, css string
              The string can be either one of:
              A standard name (Taken from the CSS specification)
              A hexadecimal value in the form “\\\\rgb”, “\\\\rrggbb”, “\\\\rrrgggbbb” or ”\\\\rrrrggggbbbb”
              A hexadecimal value in the form “\\\\rgba”, “\\\\rrggbbaa”, or ”\\\\rrrrggggbbbbaaaa”
              A RGB color in the form “rgb(r,g,b)” (In this case the color will have full opacity)
              A RGBA color in the form “rgba(r,g,b,a)”
              A HSL color in the form \"hsl(hue, saturation, lightness)\"
              A HSLA color in the form \"hsla(hue, saturation, lightness, alpha)\""
  [arg]
  (if (m/validate [:sequential number?] arg)
    (let [[red green blue alpha] arg]
      (RGBA. red green blue alpha))
    (if (map? arg)
      (let [{:keys [red green blue alpha]} arg]
        (RGBA. red green blue alpha))
      (if (string? arg)
        (let [rgba (RGBA.)]
          (if (.parse rgba ^String arg)
            rgba
            (throw (Exception. "failed to parse RGBA string"))))
        (throw (Exception. "unsupported type"))))))

(defn ^UserScript UserScript:from-edn
  [{:keys [src injectedFrames injectionTime allowList blocklist] :as arg}]
  (UserScript. src
               (UserContentInjectedFrames/valueOf injectedFrames)
               (UserScriptInjectionTime/valueOf injectionTime)
               allowList
               blocklist))

(defn ^UserStyleSheet UserStyleSheet:from-edn
  [{:keys [src injectedFrames level allowList blocklist] :as arg}]
  (UserStyleSheet. src
                   (UserContentInjectedFrames/valueOf injectedFrames)
                   (UserStyleLevel/valueOf level)
                   allowList
                   blocklist))

(defn ^WebView WebView:from-edn
  ([cfg]
   (WebView:from-edn (WebView.) cfg))
  ([instance {:keys [uri backgroundColor html scripts stylesheets
                     onScriptMessageReceived scriptMessageHandlers] :as arg}]
   (assert (instance? WebView instance))
   (let [manager (.getUserContentManager instance)]
     (when scripts
       (doseq [script scripts]
         (.addScript manager (UserScript:from-edn script))))
     (when stylesheets
       (doseq [stylesheet stylesheets]
         (.addStyleSheet manager (UserStyleSheet:from-edn stylesheet))))
     (when (seq scriptMessageHandlers)
       (doseq [h scriptMessageHandlers]
         (.registerScriptMessageHandler manager (:name h) (:worldName h))))
     (when onScriptMessageReceived
       (.onScriptMessageReceived manager nil onScriptMessageReceived)))
   (when (some? backgroundColor)
     (.setBackgroundColor instance (RGBA:from-edn backgroundColor)))
   (when uri
     (.loadUri instance uri))
   (when html
     (.loadHtml instance html nil))
   instance))

#!----------------------------------------------------------------------------------------------------------------------

(defn launch!
  "Launches a new window configured via maps.

   window-conf keys: :title, :width, :height, :decorated (default true)
   webview-conf keys: :uri

   Usage:
   (launch! \"main\" {:title \"Clojure\" :width 800 :height 600}
                     {:uri \"https://clojure.org\"})"
  ([window-name] (launch! window-name {} {}))
  ([window-name window-conf] (launch! window-name window-conf {}))
  ([window-name window-conf webview-conf]
   (if (contains? @windows window-name)
     (println (str "Error: Window '" window-name "' already exists. Close it first or use a unique name."))
     (let [app (ensure-app-running!)]
       (on-ui
         (let [window  (ApplicationWindow. app)
               _       (.realize window)  ;; do before applying config; prevents crash on setDecorated false
               window  (ApplicationWindow:from-edn window window-conf)
               webview (WebView:from-edn webview-conf)]
           (.onCloseRequest window
                            (fn []
                              (swap! windows dissoc window-name)
                              (println (str "Unregistered window [" window-name "]"))
                              false))
           (.setChild window webview)
           (.present window)
           (swap! windows assoc window-name window)
           (println (str "Launched window [" window-name "]"))))))))

(defn close! [window-name]
  (if-let [win (@windows window-name)]
    (on-ui
      (.close win)
      ;; Note: swap! dissoc happens in the onCloseRequest listener
      (println (str "Closing window [" window-name "] ...")))
    (println "Window not found.")))

(defn close-all! []
  (doseq [name (keys @windows)]
    (close! name)))

(defn fade-to! [window target-opacity duration-ms]
  (let [start-opacity (.getOpacity window)
        frames        (max 1 (/ duration-ms 8.0))
        step          (/ (- target-opacity start-opacity) frames)]
    (GLib/timeoutAdd GLib/PRIORITY_DEFAULT 16
                     (fn []
                       (let [current   (.getOpacity window)
                             next      (+ current step)
                             ;; Check if we are "close enough" or passed the target
                             finished? (if (pos? step)
                                         (>= next target-opacity)
                                         (<= next target-opacity))]

                         (if finished?
                           (do
                             (.setOpacity window target-opacity)
                             false)                         ;; Return false to stop the timer
                           (do
                             (.setOpacity window next)
                             true)))))))                    ;; Return true to keep running

(defn quit-app! []
  (when-let [app @the-app]
    (on-ui (.quit app))
    (reset! the-app nil)
    (reset! windows {})
    (println "Application quit requested.")))


(comment
  (do (require :reload 'pkpkpk.webkit) (in-ns 'pkpkpk.webkit))

  (launch! "main" {:title "Clojure Home"} {:uri "https://clojure.org"})

  (launch! "frosty"
           {:title       "hola"
            :width       800
            :height      600
            :decorated   false
            :transparent true}
           {:html            "<!DOCTYPE html><html><head><title>Javascript Demo</title></head><body></body></html>"
            :backgroundColor "rgba(0,0,0,0)"
            :stylesheets     [{:src            "body {backdrop-filter: blur(20px);background-color: rgba(255, 255, 255, 0.1);}"
                               :injectedFrames "ALL_FRAMES"
                               :level          "USER"}]})
  )
