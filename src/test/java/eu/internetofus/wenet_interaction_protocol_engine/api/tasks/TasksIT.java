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
import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.InteractionsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.InteractionsRepositoryIT;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepository;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepositoryIT;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
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

    StoreServices.storeTaskExample(43, vertx, testContext).onSuccess(task -> {
      final List<Interaction> interactions = new ArrayList<>();
      testContext.assertComplete(InteractionsRepositoryIT.storeSomeInteractions(vertx, testContext, interaction -> {
        if (interactions.size() % 2 == 0) {

          for (var i = 0; i < 10; i++) {

            if (i % 2 == 0) {

              interaction.taskId = task.id;
            }
          }
        }

      }, 20, interactions)).onSuccess(any -> {

        final List<State> states = new ArrayList<>();
        testContext.assertComplete(StatesRepositoryIT.storeSomeStates(vertx, testContext, state -> {
          if (states.size() % 2 == 0) {

            for (var i = 0; i < 10; i++) {

              if (i % 2 == 0) {

                state.taskId = task.id;
              }
            }
          }

        }, 20, states)).onSuccess(any2 -> {

          testContext.assertComplete(WeNetInteractionProtocolEngine.createProxy(vertx).taskDeleted(task.id))
              .onSuccess(empty -> this.assertDeletedTaskInfo(task.id, interactions, states, vertx, testContext));
        });
      });
    });
  }

  /**
   * Check that the task information has been removed.
   *
   * @param taskId       identifier of the task.
   * @param interactions that may be deleted.
   * @param states       that may be deleted.
   * @param vertx        event bus to use.
   * @param testContext  context that executes the test.
   */
  private void assertDeletedTaskInfo(final String taskId, final List<Interaction> interactions,
      final List<State> states, final Vertx vertx, final VertxTestContext testContext) {

    if (states.isEmpty() && interactions.isEmpty()) {

      testContext.completeNow();

    } else if (interactions.isEmpty()) {

      final var expected = states.remove(0);
      if (expected.taskId.equals(taskId)) {

        testContext
            .assertFailure(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onFailure(ignored -> {

              this.assertDeletedTaskInfo(taskId, interactions, states, vertx, testContext);

            });

      } else {

        testContext
            .assertComplete(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onSuccess(updated -> {

              testContext.verify(() -> {

                assertThat(updated).isEqualTo(expected);

              });

              this.assertDeletedTaskInfo(taskId, interactions, states, vertx, testContext);

            });
      }

    } else {

      final var expected = interactions.remove(0);
      final var context = new ModelsPageContext();
      context.limit = 100;
      context.offset = 0;
      context.query = new JsonObject().put("taskId", expected.taskId);
      testContext.assertComplete(InteractionsRepository.createProxy(vertx).retrieveInteractionsPage(context))
          .onSuccess(page -> {

            testContext.verify(() -> {

              if (expected.taskId.equals(taskId)) {

                assertThat(page.total).isEqualTo(0);

              } else {

                assertThat(page.total).isGreaterThanOrEqualTo(1);
                assertThat(page.interactions).isNotNull().contains(expected);

              }

            });

            this.assertDeletedTaskInfo(taskId, interactions, states, vertx, testContext);

          });
    }

  }

}
