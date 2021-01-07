/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.tasks;

import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.TaskType;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import eu.internetofus.wenet_interaction_protocol_engine.HardCodedProtocolWorker;
import eu.internetofus.wenet_interaction_protocol_engine.MessageForWorkerBuilder;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import javax.ws.rs.core.Response.Status;

/**
 * The implementation of the web services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class TasksResource implements Tasks {

  /**
   * The component used to store a cache of the task types.
   */
  private static final Map<String, TaskType> typesCache = Collections.synchronizedMap(new WeakHashMap<>());

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new resource.
   *
   * @param vertx event bus to use.
   */
  public TasksResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * Return the task type associated to a task.
   *
   * @param taskId identifier of the task to obtain the task type.
   *
   * @return the future that will provide the task type associated to the task.
   */
  protected Future<TaskType> getTypeOf(final String taskId) {

    final Promise<TaskType> promise = Promise.promise();
    final var type = typesCache.get(taskId);
    if (type != null) {

      promise.complete(type);

    } else {

      WeNetTaskManager.createProxy(this.vertx).retrieveTask(taskId).onComplete(retrieve -> {

        final var result = retrieve.result();
        if (result != null) {

          promise.handle(this.getTypeOf(result));

        } else {

          promise.fail(retrieve.cause());
        }

      });
    }

    return promise.future();

  }

  /**
   * Return the task type associated to a task.
   *
   * @param task to obtain the task type.
   *
   * @return the future that will provide the task type associated to the task.
   */
  protected Future<TaskType> getTypeOf(final Task task) {

    final Promise<TaskType> promise = Promise.promise();
    final var type = typesCache.get(task.id);
    if (type != null) {

      promise.complete(type);

    } else {

      WeNetTaskManager.createProxy(this.vertx).retrieveTaskType(task.taskTypeId).onComplete(retrieve -> {

        final var result = retrieve.result();
        if (result != null) {

          typesCache.put(task.id, result);
          promise.complete(result);

        } else {

          promise.fail(retrieve.cause());
        }

      });
    }

    return promise.future();

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void taskCreated(final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = new ModelContext<Task, String>();
    model.name = "task";
    model.type = Task.class;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.toModel(body, model, context, () -> {

      ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, model.source);

      this.getTypeOf(model.source).onComplete(get -> {

        final var message = MessageForWorkerBuilder.buildCreatedTaskMessage(model.source);
        final var type = get.result();
        if (type == null || type.norms == null || type.norms.isEmpty()) {
          // it is a hard-coded protocol

          this.vertx.eventBus().publish(HardCodedProtocolWorker.ADDRESSS, message);

        } else {
          // process the message
          this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

        }
      });
    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void doTransaction(final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = new ModelContext<TaskTransaction, String>();
    model.name = "taskTransaction";
    model.type = TaskTransaction.class;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.toModel(body, model, context, () -> {

      ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, model.source);

      this.getTypeOf(model.source.taskId).onComplete(get -> {

        final var type = get.result();
        final var message = MessageForWorkerBuilder.buildDoTaskTransactionMessage(model.source);

        if (type == null || type.norms == null || type.norms.isEmpty()) {
          // it is a hard-coded protocol
          this.vertx.eventBus().publish(HardCodedProtocolWorker.ADDRESSS, message);

        } else {
          // process the message
          this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

        }
      });

    });

  }

}
