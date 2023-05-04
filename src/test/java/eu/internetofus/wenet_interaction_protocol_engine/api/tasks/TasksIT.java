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

import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.models.MessageTest;
import eu.internetofus.common.components.models.TaskTransactionTest;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * The integration test over the {@link Tasks}.
 *
 * @see Tasks
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class TasksIT {

  /**
   * Verify that remove information about a task.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Tasks#taskDeleted(String, io.vertx.ext.web.api.service.ServiceRequest,
   *      Handler)
   */
  @Test
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  public void shouldNotifiedDeletedTask(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    StoreServices.storeTaskExample(1, vertx, testContext).onSuccess(task1 -> {

      final var userId = task1.requesterId;
      StoreServices.storeTaskExample(2, vertx, testContext).onSuccess(task2 -> {

        task2.transactions = new ArrayList<>();
        for (var i = 0; i < 10; i++) {

          final var transaction = new TaskTransactionTest().createModelExample(i);
          if (i % 2 == 0) {

            transaction.actioneerId = userId;
          }

          for (var j = 0; j < 10; j++) {

            final var message = new MessageTest().createModelExample(j);
            if (j % 2 == 0) {

              message.receiverId = userId;
            }
            transaction.messages.add(message);

          }

        }
        testContext.assertComplete(WeNetTaskManager.createProxy(vertx).updateTask(task2.id, task2))
            .onSuccess(updatedTask2 -> {

              testContext.assertComplete(WeNetInteractionProtocolEngine.createProxy(vertx).taskDeleted(userId))
                  .onSuccess(empty -> {

                    vertx.setTimer(10000, ignoredTimer -> {

                      testContext.assertFailure(WeNetTaskManager.createProxy(vertx).retrieveTask(task1.id))
                          .onFailure(ignoredError -> {

                            testContext.assertComplete(WeNetTaskManager.createProxy(vertx).retrieveTask(task2.id))
                                .onSuccess(updatedAfterDeleteTask2 -> testContext.verify(() -> {

                                  assertThat(updatedAfterDeleteTask2.transactions).isNotNull();
                                  for (final var transaction : updatedAfterDeleteTask2.transactions) {

                                    assertThat(transaction.actioneerId).isNotEqualTo(userId);
                                    assertThat(transaction.messages).isNotNull();
                                    for (final var message : transaction.messages) {

                                      assertThat(message.receiverId).isNotEqualTo(userId);

                                    }
                                  }

                                  testContext.completeNow();

                                }));

                          });
                    });

                  });
            });
      });

    });
  }

}
