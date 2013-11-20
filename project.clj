(defproject page-signal "0.1.0"
  :description "Library to extract article text from web pages."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx1g"]
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [enlive "1.1.1"]
                 [criterium "0.3.1"]
                 [incanter "1.4.1"]
                 [de.l3s.boilerpipe "1.2.0"]
                 [xerces/xercesImpl "2.9.1"]
                 [net.sourceforge.nekohtml/nekohtml "1.9.15"]])
