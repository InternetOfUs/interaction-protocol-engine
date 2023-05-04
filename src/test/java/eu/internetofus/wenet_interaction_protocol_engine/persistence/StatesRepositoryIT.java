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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.model.Model;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the {@link StatesRepository}.
 *
 * @see StatesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class StatesRepositoryIT {

  /**
   * Verify that can not found a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldNotFoundUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertFailure(StatesRepository.createProxy(vertx).searchState("undefined", "undefined", "undefined"))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Create an state that is unique.
   *
   * @return the created state.
   */
  protected State createUniqueState() {

    final var state = new State();
    state.attributes = new JsonObject().put("key", "value");
    state.communityId = UUID.randomUUID().toString();
    state.taskId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    return state;

  }

  /**
   * Verify that can found a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldFoundState(final Vertx vertx, final VertxTestContext testContext) {

    final var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state)).onSuccess(storedState -> {
      testContext.assertComplete(
          StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)).onSuccess(

              foundState -> testContext.verify(() -> {
                assertThat(foundState).isEqualTo(storedState);
                testContext.completeNow();
              }));
    });

  }

  /**
   * Verify that can not store a state that can not be an object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldNotStoreAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).storeState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can store a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldStoreState(final Vertx vertx, final VertxTestContext testContext) {

    final var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(storedState -> testContext.verify(() -> {

          assertThat(storedState).isEqualTo(state);
          testContext.completeNow();
        }));

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    final var state = this.createUniqueState();
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can update a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldUpdateState(final Vertx vertx, final VertxTestContext testContext) {

    final var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(stored -> testContext.verify(() -> {

          final var update = Model.fromJsonObject(stored.toJsonObject(), State.class);
          update.attributes.put("key2", "value2");
          testContext.assertComplete(StatesRepository.createProxy(vertx).updateState(update).compose(
              empty -> StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)))
              .onSuccess(foundState -> testContext.verify(() -> {

                assertThat(foundState._creationTs).isEqualTo(stored._creationTs);
                assertThat(foundState._lastUpdateTs).isGreaterThanOrEqualTo(stored._lastUpdateTs);
                update._creationTs = stored._creationTs;
                update._lastUpdateTs = foundState._lastUpdateTs;
                assertThat(foundState).isEqualTo(update);
                testContext.completeNow();
              }));

        }));

  }

  /**
   * Create some {@link State}.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   * @param change      function to modify the pattern before to store it.
   * @param max         number maximum of states to create.
   * @param states      list to add the created states.
   *
   * @return the future creation result.
   */
  public Future<Void> storeSomeStates(final Vertx vertx, final VertxTestContext testContext,
      final Consumer<State> change, final int max, final List<State> states) {

    if (states.size() == max) {

      return Future.succeededFuture();

    } else {

      final var state = this.createUniqueState();
      change.accept(state);
      return testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state)).compose(stored -> {
        states.add(stored);
        return this.storeSomeStates(vertx, testContext, change, max, states);
      });
    }

  }

  /**
   * Check that delete all the states with the specified user.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  @Test
  public void shouldDeleteAllStateByUser(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(43, vertx, testContext).onSuccess(profile -> {
      final List<State> states = new ArrayList<>();
      testContext.assertComplete(this.storeSomeStates(vertx, testContext, state -> {
        if (states.size() % 2 == 0) {

          for (var i = 0; i < 10; i++) {

            if (i % 2 == 0) {

              state.userId = profile.id;
            }
          }
        }

      }, 20, states)).onSuccess(any -> {

        testContext.assertComplete(StatesRepository.createProxy(vertx).deleteAllStateByUser(profile.id))
            .onSuccess(empty -> this.assertDeletedStatesByUser(profile.id, states, vertx, testContext));
      });
    });

  }

  /**
   * Check that the states of an user has been removed.
   *
   * @param profileId   identifier of the user.
   * @param states      where the states has to be removed.
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  private void assertDeletedStatesByUser(final String profileId, final List<State> states, final Vertx vertx,
      final VertxTestContext testContext) {

    if (states.isEmpty()) {

      testContext.completeNow();

    } else {

      final var expected = states.remove(0);
      if (expected.userId.equals(profileId)) {

        testContext
            .assertFailure(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onFailure(ignored -> {

              this.assertDeletedStatesByUser(profileId, states, vertx, testContext);

            });

      } else {

        testContext
            .assertComplete(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onSuccess(updated -> {

              testContext.verify(() -> {

                assertThat(updated).isEqualTo(expected);

              });

              this.assertDeletedStatesByUser(profileId, states, vertx, testContext);

            });
      }
    }
  }

  /**
   * Check that delete all the states with the specified task.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  @Test
  public void shouldDeleteAllStateByTask(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeTaskExample(43, vertx, testContext).onSuccess(task -> {
      final List<State> states = new ArrayList<>();
      testContext.assertComplete(this.storeSomeStates(vertx, testContext, state -> {
        if (states.size() % 2 == 0) {

          for (var i = 0; i < 10; i++) {

            if (i % 2 == 0) {

              state.taskId = task.id;
            }
          }
        }

      }, 20, states)).onSuccess(any -> {

        testContext.assertComplete(StatesRepository.createProxy(vertx).deleteAllStateByTask(task.id))
            .onSuccess(empty -> this.assertDeletedStatesByTask(task.id, states, vertx, testContext));
      });
    });

  }

  /**
   * Check that the states of an task has been removed.
   *
   * @param taskId      identifier of the task.
   * @param states      where the states has to be removed.
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   */
  private void assertDeletedStatesByTask(final String taskId, final List<State> states, final Vertx vertx,
      final VertxTestContext testContext) {

    if (states.isEmpty()) {

      testContext.completeNow();

    } else {

      final var expected = states.remove(0);
      if (expected.taskId.equals(taskId)) {

        testContext
            .assertFailure(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onFailure(ignored -> {

              this.assertDeletedStatesByTask(taskId, states, vertx, testContext);

            });

      } else {

        testContext
            .assertComplete(
                StatesRepository.createProxy(vertx).searchState(expected.communityId, expected.taskId, expected.userId))
            .onSuccess(updated -> {

              testContext.verify(() -> {

                assertThat(updated).isEqualTo(expected);

              });

              this.assertDeletedStatesByTask(taskId, states, vertx, testContext);

            });
      }
    }
  }

}
