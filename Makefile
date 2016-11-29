#Packaging for standalone deployment

distribute:
	sbt sparkFatJar/assembly
	mkdir -p ./jar/
	cp -aR ./subpojects/spark-fat-jar/target/scala-2.11/sparkFatJar-assembly-0.1-SNAPSHOT.jar ./jar/qb-pong-1.0.jar

build: distribute

tarball: clean distribute
	( cd jar && tar -zcvf qb-bot.tar.gz ./ )

tarball-dist: clean distribute build tarball

clean:
	rm -rf ./jar
	rm -f ./qb-bot.tar.gz
