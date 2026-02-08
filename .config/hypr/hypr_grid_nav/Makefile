PLUGIN_NAME = hypr-grid-nav

CXX      = g++
CXXFLAGS = -std=c++23 -fPIC -Wall -Wextra -O2 \
           $(shell pkg-config --cflags hyprland) \
           -Isubmodules/hyprlang/include

LDFLAGS  = -shared \
           $(shell pkg-config --libs hyprland)

SRCS = main.cc

TARGET = lib$(PLUGIN_NAME).so

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean install clangd
