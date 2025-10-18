#pragma once

#include <QRunnable>
// #include <function>

namespace App {

using SubMapTaskFn = std::function<void()>;

class SubMapTask : public QRunnable {
 public:
  explicit SubMapTask(SubMapTaskFn const &task) : task(task) {}

  void run() override { task(); }

 private:
  const SubMapTaskFn task;
};

}  // namespace App
