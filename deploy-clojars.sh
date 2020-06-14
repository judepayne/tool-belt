clojure -A:jar toolbelt.jar
mvn deploy:deploy-file -Dfile=toolbelt.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/
