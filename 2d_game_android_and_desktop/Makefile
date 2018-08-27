# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

MODE:=debug

.PHONY: standalone
standalone:
	castle-engine --mode=$(MODE) compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: win32
win32:
	castle-engine --mode=$(MODE) package --os=win32 --cpu=i386

.PHONY: win64
win64:
	castle-engine --mode=$(MODE) package --os=win64 --cpu=x86_64

.PHONY: linux32
linux32:
	castle-engine --mode=$(MODE) package --os=linux --cpu=i386

.PHONY: linux64
linux64:
	castle-engine --mode=$(MODE) package --os=linux --cpu=x86_64

.PHONY: src
src:
	castle-engine --mode=$(MODE) package-source

.PHONY: android
android:
	castle-engine --mode=$(MODE) package --os=android --cpu=arm --fast
	castle-engine install --os=android --cpu=arm
	castle-engine run --os=android --cpu=arm

.PHONY: android-release
android-release:
	castle-engine clean
	$(MAKE) android MODE=release

.PHONY: ios
ios:
	castle-engine --mode=$(MODE) package --target=iOS --fast

.PHONY: ios-release
ios-release:
	castle-engine clean
	$(MAKE) ios MODE=release
