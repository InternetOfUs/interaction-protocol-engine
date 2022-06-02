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
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.service.MessagePredicates;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.model.TimeManager;
import eu.internetofus.common.protocols.AbstractProtocolITC;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.UUID;
import java.util.function.Predicate;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test the event actions.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class EventActionsIT extends AbstractProtocolITC {

  /**
   * Create the protocol.
   *
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    final var resource = this.getClass().getName().replaceAll("\\.", "/") + "_taskType.json";
    return this.loadTaskTypeForProtocol(resource, vertx, testContext);

  }

  /**
   * {@inheritDoc}
   *
   * @return {@code 1} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 1;
  }

  /**
   * The delay to apply for the notifications.
   *
   * @return the seconds to delay the notifications of the protocol.
   */
  protected long getNotificationDelay() {

    return 10L;
  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(5)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(4, testContext);

    final var now = TimeManager.now();
    final var source = this.createTaskForProtocol();
    final var notificationDelay = this.getNotificationDelay();
    source.attributes = new JsonObject().put("notificationDelay", notificationDelay);

    final var checkMessages = new ArrayList<Predicate<Message>>();
    checkMessages.add(this.createMessagePredicate().and(MessagePredicates.labelIs("taskCreated"))
        .and(MessagePredicates.receiverIs(source.requesterId)).and(MessagePredicates.attributesAre(target -> {

          final var attPreviousNow = target.getLong("previousNow", -1L);
          final var attNow = target.getLong("now", -1L);
          return now <= attPreviousNow && attPreviousNow + notificationDelay <= attNow;

        })));

    final var createTransaction = new TaskTransaction();
    createTransaction.label = TaskTransaction.CREATE_TASK_LABEL;
    createTransaction.actioneerId = source.requesterId;
    final var checkTask = this.createTaskPredicate().and(TaskPredicates.similarTo(source))
        .and(TaskPredicates.transactionSizeIs(1))
        .and(TaskPredicates.transactionAt(0,
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(createTransaction))
                .and(TaskTransactionPredicates.messagesSizeIs(checkMessages.size()))
                .and(TaskTransactionPredicates.containsMessages(checkMessages))));

    final Future<?> future = this.waitUntilTaskCreated(source, vertx, testContext, checkTask)
        .compose(ignored -> this.waitUntilCallbacks(vertx, testContext, checkMessages));
    future.onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

  /**
   * Check that send an event using the transaction.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  public void shouldSendEvent(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var now = TimeManager.now();
    final var notificationDelay = this.getNotificationDelay();

    final var transaction = new TaskTransaction();
    transaction.actioneerId = this.task.requesterId;
    transaction.taskId = this.task.id;
    transaction.label = "sendEvent";
    final var content = UUID.randomUUID().toString();
    transaction.attributes = new JsonObject().put("content", content);

    final var checkMessages = new ArrayList<Predicate<Message>>();
    final var checkMessage = this.createMessagePredicate().and(MessagePredicates.labelIs("eventSent"))
        .and(MessagePredicates.receiverIs(this.task.requesterId)).and(MessagePredicates.attributesAre(target -> {

          final var attPreviousNow = target.getLong("previousNow", -1L);
          final var attNow = target.getLong("now", -1L);
          return now <= attPreviousNow && attPreviousNow + notificationDelay <= attNow
              && content.equals(target.getString("content"));

        }));
    checkMessages.add(checkMessage);

    final var transactionCheck = TaskPredicates.lastTransactionIs(this.createTaskTransactionPredicate()
        .and(TaskTransactionPredicates.similarTo(transaction)).and(TaskTransactionPredicates.messagesSizeIs(1))
        .and(TaskTransactionPredicates.messageAt(0, checkMessage)));

    final var checkTask = this.createTaskPredicate().and(TaskPredicates.transactionSizeIs(2)).and(transactionCheck);
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(transaction)
        .compose(any -> this.waitUntilTask(vertx, testContext, checkTask))
        .compose(ignored -> this.waitUntilCallbacks(vertx, testContext, checkMessages));
    future.onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

  /**
   * Check that send an event using the transaction.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(7)
  public void shouldNotSendEventBecauseCancelled(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var now = new long[] { TimeManager.now() };
    final var notificationDelay = this.getNotificationDelay();

    final var sendEventTransaction = new TaskTransaction();
    sendEventTransaction.actioneerId = this.task.requesterId;
    sendEventTransaction.taskId = this.task.id;
    sendEventTransaction.label = "sendEvent";
    sendEventTransaction.attributes = new JsonObject().put("content", UUID.randomUUID().toString());

    final var cancelEventTransaction = new TaskTransaction();
    cancelEventTransaction.actioneerId = this.task.requesterId;
    cancelEventTransaction.taskId = this.task.id;
    cancelEventTransaction.label = "cancelLastEvent";

    final var checkMessages = new ArrayList<Predicate<Message>>();
    checkMessages.add(this.createMessagePredicate().and(target -> {
      // check the sendEvent is cancelled
      return !"eventSent".equals(target.label);

    }));

    final var checkMessage = this.createMessagePredicate().and(MessagePredicates.labelIs("eventSent"))
        .and(MessagePredicates.receiverIs(this.task.requesterId)).and(MessagePredicates.attributesAre(target -> {

          final var success = target.getBoolean("success", false);
          return success;

        }));
    checkMessages.add(checkMessage);

    final var currentTransactionsSize = this.task.transactions.size();
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(sendEventTransaction)
        .compose(
            any -> this.waitUntilTask(vertx, testContext,
                this.createTaskPredicate().and(TaskPredicates.transactionAt(currentTransactionsSize,
                    this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(sendEventTransaction))
                        .and(TaskTransactionPredicates.withoutMessages())))))
        .compose(any -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(cancelEventTransaction)).compose(any -> {

          now[0] = TimeManager.now();
          return Future.succeededFuture();

        })
        .compose(any -> this.waitUntilTask(vertx, testContext, this.createTaskPredicate()
            .and(TaskPredicates.transactionSizeIs(currentTransactionsSize + 2))
            .and(TaskPredicates.transactionAt(currentTransactionsSize + 1,
                this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(cancelEventTransaction))
                    .and(TaskTransactionPredicates.messagesSizeIs(1))
                    .and(TaskTransactionPredicates.messageAt(0, checkMessage))))))
        .compose(ignored -> this.waitUntilCallbacks(vertx, testContext, checkMessages)).map(ignored -> {

          if (TimeManager.now() < now[0] + notificationDelay) {

            testContext.failNow("Message not delayed");
          }
          return null;
        });
    future.onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
