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
package eu.internetofus.wenet_interaction_protocol_engine.prolog;

import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.service.MessagePredicates;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.protocols.AbstractProtocolITC;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.tinylog.Logger;

/**
 * Generic test to check some conditions.
 *
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public abstract class AbstractConditionsITC extends AbstractProtocolITC {

  /**
   * Create the protocol.
   *
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    final var taskType = new TaskType();
    taskType.callbacks = new JsonObject().put("success", new JsonObject().put("type", "object").put("properties",
        new JsonObject().put("condition", new JsonObject().put("type", "string"))));
    taskType.name = "Testing " + this.getClass().getSimpleName();
    taskType.norms = new ArrayList<>();
    taskType.norms.add(new ProtocolNorm());
    taskType.norms.get(0).priority = 100000;
    taskType.norms.get(0).description = "Test condition: ";
    taskType.norms.get(0).whenever = "is_received_created_task()";
    taskType.norms.get(0).thenceforth = "add_created_transaction()";

    for (final var condition : this.getSucessConditionsToTest()) {

      final var norm = new ProtocolNorm();
      taskType.norms.add(norm);
      norm.priority = 100000 - taskType.norms.size();
      norm.description = "Test condition: " + condition;
      norm.whenever = condition;
      norm.thenceforth = "send_user_message('success',json([condition=\"" + condition.replaceAll("\"", "''") + "\"]))";

    }

    return Future.succeededFuture(taskType);
  }

  /**
   * Return the conditions that are {@code true} and has to be test.
   *
   * @return the list of condition that has to be {@code true}.
   */
  public abstract List<String> getSucessConditionsToTest();

  /**
   * Wait until has received the specified callbacks.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return the messages that satisfy the predicates.
   */
  protected Future<List<Message>> waitUntilCallbacks(@NotNull final Vertx vertx,
      @NotNull final VertxTestContext testContext) {

    final var conditions = this.getSucessConditionsToTest();
    return this.waitUntil(vertx, testContext, () -> Model
        .fromFutureJsonArray(WeNetServiceSimulator.createProxy(vertx).retrieveCallbacks(this.app.appId), Message.class),
        callbacks -> {

          final var conditionsIter = conditions.iterator();
          while (conditionsIter.hasNext()) {

            final var condition = conditionsIter.next();
            var matchCondition = false;
            final var checkMessage = this.createMessagePredicate().and(MessagePredicates.labelIs("success"))
                .and(MessagePredicates
                    .attributesAre(new JsonObject().put("condition", condition.replaceAll("\"", "''"))))
                .and(MessagePredicates.receiverIs(this.users.get(0).id));
            final var callbacksIter = callbacks.iterator();
            while (callbacksIter.hasNext()) {

              final var msg = callbacksIter.next();
              if (msg != null && checkMessage.test(msg)) {
                matchCondition = true;
                callbacksIter.remove();
                break;
              }
            }

            if (!matchCondition) {

              Logger.warn("Not match for: {}", condition);
              return false;

            } else {

              Logger.info("Matched for: {}", condition);
              conditionsIter.remove();

            }
          }

          return true;

        }).compose(callbacks -> WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(this.app.appId)
            .map(empty -> callbacks));

  }

  /**
   * Do nothing. Only for extended class add necessary data.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(5)
  public void shouldPrepareEnvironmentToCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(4, testContext);
    this.assertSuccessfulCompleted(testContext);

  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var source = this.createTaskForProtocol();

    final var createTransaction = new TaskTransaction();
    createTransaction.label = TaskTransaction.CREATE_TASK_LABEL;
    createTransaction.actioneerId = source.requesterId;
    final var checkTask = this.createTaskPredicate().and(TaskPredicates.similarTo(source))
        .and(TaskPredicates.transactionSizeIs(1)).and(TaskPredicates.transactionAt(0,
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(createTransaction))));

    this.waitUntilTaskCreated(source, vertx, testContext, checkTask)
        .compose(ignored -> this.waitUntilCallbacks(vertx, testContext))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
