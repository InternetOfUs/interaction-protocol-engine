/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.tasks;

import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import eu.internetofus.wenet_interaction_protocol_engine.HardCodedProtocolWorker;
import eu.internetofus.wenet_interaction_protocol_engine.MessageForWorkerBuilder;
import eu.internetofus.wenet_interaction_protocol_engine.ProtocolData;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import javax.ws.rs.core.Response.Status;
import org.tinylog.Logger;

/**
 * The implementation of the web services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class TasksResource implements Tasks {

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

      ProtocolData.createWith(model.source, this.vertx).onSuccess(protocol -> {

        if (!protocol.hasProtocolNorms()) {
          // it is a hard-coded protocol
          final var message = MessageForWorkerBuilder.buildCreatedTaskMessage(model.source);
          this.vertx.eventBus().publish(HardCodedProtocolWorker.ADDRESSS, message);

        } else {
          // process the message
          final var message = MessageForWorkerBuilder.buildProtocolMessageForCreatedTask(protocol);
          this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

        }

      }).onFailure(cause -> {

        Logger.warn(cause, "Cannot process the creation of the task {}", model.source);

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

      ProtocolData.createWith(model.source, this.vertx).onSuccess(protocol -> {

        if (!protocol.hasProtocolNorms()) {
          // it is a hard-coded protocol
          final var message = MessageForWorkerBuilder.buildDoTaskTransactionMessage(model.source);
          this.vertx.eventBus().publish(HardCodedProtocolWorker.ADDRESSS, message);

        } else {
          // process the message
          final var message = MessageForWorkerBuilder.buildProtocolMessageForDoTaskTransaction(model.source, protocol);
          this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

        }

      }).onFailure(cause -> {

        Logger.warn(cause, "Cannot process the creation of the task {}", model.source);

      });

    });

  }

}
