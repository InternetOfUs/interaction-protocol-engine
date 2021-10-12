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

import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.protocols.AbstractProtocolITC;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over any of the prolog predicates.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public abstract class AbstractPrologITC extends AbstractProtocolITC {

  /**
   * {@inheritDoc}
   *
   * @return {@code 10} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 10;
  }

  /**
   * Create a task type with the basic type that send a message to the user when
   * the task is created.
   *
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    final var taskType = new TaskType();
    taskType.name = "Testing " + this.getClass().getSimpleName();
    taskType.norms = new ArrayList<>();
    taskType.norms.add(new ProtocolNorm());
    taskType.norms.get(0).description = "Do the action when the protocol is created";
    final var wheneverCode = this.getWheneverCode();
    taskType.norms.get(0).whenever = "is_received_created_task() and " + wheneverCode;
    final var thenceforthCode = this.getThenceforthCode();
    taskType.norms.get(0).thenceforth = "add_created_transaction() and " + thenceforthCode;

    return Future.succeededFuture(taskType);
  }

  /**
   * Return the code to add the condition of the created norm.
   *
   * @return the condition code to test.
   */
  protected abstract String getWheneverCode();

  /**
   * Return the code to add the actions of the created norm.
   *
   * @return the action code to test.
   */
  protected abstract String getThenceforthCode();

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

    final var source = this.createTaskForProtocol();

    final var createTransaction = new TaskTransaction();
    createTransaction.label = TaskTransaction.CREATE_TASK_LABEL;
    createTransaction.actioneerId = source.requesterId;
    final var checkTask = this.createTaskPredicate().and(TaskPredicates.similarTo(source))
        .and(TaskPredicates.transactionSizeIs(1)).and(TaskPredicates.transactionAt(0,
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(createTransaction))));

    this.doBeforeTaskCreated(vertx, testContext)
        .compose(ignored -> this.waitUntilTaskCreated(source, vertx, testContext, checkTask))
        .compose(ignored -> this.doAfterTaskCreated(vertx, testContext))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

  /**
   * The future do add the necessary components before create the task
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return future to prepare the context to create the task.
   */
  protected abstract Future<?> doBeforeTaskCreated(Vertx vertx, VertxTestContext testContext);

  /**
   * The future to check the created task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   *
   * @return future to validate the created task.
   */
  protected abstract Future<?> doAfterTaskCreated(Vertx vertx, VertxTestContext testContext);

}
