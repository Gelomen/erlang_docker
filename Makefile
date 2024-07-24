ALL: build_a build_b scp

deps:
	./rebar3 get-deps

co:
	./rebar3 compile

run:
	./rebar3 shell

clean:
	rm -rf _build/

release_a: clean
	./rebar3 as a release
	./rebar3 as a tar

release_b: clean
	./rebar3 as b release
	./rebar3 as b tar

image_dir:
	mkdir -p docker_images

build_a: image_dir
	@echo "==> docker build a"
	@docker build --build-arg VERSION="a" -t erlang-docker-a .
	@docker save -o ./docker_images/erlang-docker-a.tar erlang-docker-a

build_b: image_dir
	@echo "==> docker build b"
	@docker build --build-arg VERSION="b" -t erlang-docker-b .
	@docker save -o ./docker_images/erlang-docker-b.tar erlang-docker-b

scp: scp_images scp_yaml scp_script

scp_images:
	@echo "==> scp images"
	@scp -r docker_images/* master:~

scp_yaml:
	@echo "==> scp yaml"
	@scp -r k8s_yaml/ master:~

scp_script:
	@echo "==> scp script"
	@scp update.sh master:~

worker_clean:
	@echo "==> worker clean"
	@for f in 1 2 3 ; do \
		multipass exec worker$$f -- sudo docker system prune -a -f; \
	done
